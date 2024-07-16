%% lazy Kalman filtering
% Estimate the value and uncertainty of each arm using lazy Kalman filtering.
% Calculate the V, RU, and VTU values for each trial.
% Estimate parameters η a bias in the Kalman gain.
% V = Q(1) - Q(2); arm = 1 means left, arm = 2 means right
% RU = σ(1) - σ(2);
% VTU = sqrt(σ(1)^2 + σ(2)^2);

workpath = pwd;
cd('../../');
data = readtable('step2_descriptive_statistics/output/kalman_data.csv');
cd(workpath);

% initialize the V, RU, and VTU values
data.V   = zeros(height(data), 1);
data.RU  = zeros(height(data), 1);
data.VTU = zeros(height(data), 1);
data.eta = zeros(height(data), 1);
etalist  = zeros(max(data.ID), 1);

% function of lazy Kalman filtering
% and glm with V RU VTU
function [nLL, results, eta, output] = KFL(eta, input)
% eta:   scalar (0,1); individual parameter
% input: table/structure; all data of target subject
%% set the prior uncertainty at block level
%tau_0_S = std(repmat([2, 3, 4, 5, 6], 1, 4)); % mean value of the reward of safe arm and semi-risky arm
%tau_0_r = tau_0_S;
%tau_0_R = 0;
%the fewest assumptions, same prior uncertainty for all arms
tau_0 = (7 - 1) / sqrt(12); % the range of the reward of all arm
tau_0_S = tau_0;
tau_0_r = tau_0;
tau_0_R = tau_0;

% set the prior uncertainty at trial level
tau_S = 0;
tau_r = sqrt(0.595);
tau_R = sqrt(2.98);
%% Kalman part
for i = 1:20 % block level
    data_sub_block = input(input.block_number == i, :);
    if isempty(data_sub_block) % skip the block if there is no data
        continue;
    end

    Q     = [0; 0]; % Q(1) is the value of the left arm, Q(2) is the value of the right arm
    sigma = [0; 0]; % sigma(1) is the uncertainty of the left arm, sigma(2) is the uncertainty of the right arm
    alpha = [0; 0]; % learning rate
    tau   = [0; 0]; % trial uncertainty

    % we can also consider the prior value as 4 for win and -4 for lose as it is the expectation of the whole task
    context = string(data_sub_block.context(1));
    switch context
        case "Win"
            Q(1) = 4; % set prior value of arm 1
            Q(2) = 4; % set prior value of arm 2
        case "Lose"
            Q(1) = -4; % set prior value of arm 1
            Q(2) = -4; % set prior value of arm 2
    end

    left_arm = string(data_sub_block.left(1));
    switch left_arm % set prior uncertainty of arm 1
        case '**S**'
            sigma(1) = tau_0_S;
            alpha(1) = sigma(1)^2 / (sigma(1)^2 + tau_S^2);
            tau(1)   = tau_S;
        case '**r**'
            sigma(1) = tau_0_r;
            alpha(1) = sigma(1)^2 / (sigma(1)^2 + tau_r^2);
            tau(1)   = tau_r;
        case '**R**'
            sigma(1) = tau_R; % aviod zero learning rate
            alpha(1) = sigma(1)^2 / (sigma(1)^2 + tau_R^2);
            tau(1)   = tau_R;
    end

    right_arm = string(data_sub_block.right(1));
    switch right_arm % set prior uncertainty of arm 2
        case '**S**'
            sigma(2) = tau_0_S;
            alpha(2) = sigma(2)^2 / (sigma(2)^2 + tau_S^2);
            tau(2)   = tau_S;
        case '**r**'
            sigma(2) = tau_0_r;
            alpha(2) = sigma(2)^2 / (sigma(2)^2 + tau_r^2);
            tau(2)   = tau_r;
        case '**R**'
            sigma(2) = tau_R;
            alpha(2) = sigma(2)^2 / (sigma(2)^2 + tau_R^2);
            tau(2)   = tau_R;
    end

    for j = 1:12 % trial level
        data_sub_block_trial = data_sub_block(data_sub_block.trial == j, :);
        if isempty(data_sub_block_trial) % skip the trial if there is no data
            continue;
        end
        a_t = data_sub_block_trial.choice;

        Q(a_t)     = Q(a_t) + alpha(a_t) * (data_sub_block_trial.payoff - Q(a_t));
        sigma(a_t) = sqrt(sigma(a_t)^2 - alpha(a_t) * sigma(a_t)^2);

        if sigma(a_t) == 0 % aviod NAN in safe arm learning
            alpha(a_t) = 1;
        else
            % THE MAIN DIFFERENT between standard Kalman filter
            alpha(a_t) = eta * sigma(a_t)^2 / (sigma(a_t)^2 + tau(a_t)^2);
        end

        V = Q(1) - Q(2);
        data_sub_block_trial.V = V;
        data_sub_block_trial.RU = sigma(1) - sigma(2);
        TU = sqrt(sigma(1)^2 + sigma(2)^2);
        data_sub_block_trial.VTU = V / TU;
        data_sub_block(data_sub_block.trial == j, :) = data_sub_block_trial;
    end
    input(input.block_number == i, :) = data_sub_block;
end
output = input;
output.choice = 2 - output.choice; % now 1 for left 0 for right
%% GLM part
formula = 'choice ~ -1 + V + RU + VTU';
results = fitglm(output,formula,'Distribution','Binomial','Link','probit');
nLL = -results.LogLikelihood; % negative LL as objective function
end

for n = 1:max(data.ID) % individual level
    data_sub = data(data.ID == n, :);
    % Initial guess for eta
    initialEta = 1;  % Set standard KF to a reasonable initial guess

    % Set up constraints (if any)
    lb = 0;  % Lower bound for eta
    ub = 1;  % Upper bound for eta

    % Set up options for fmincon with active-set algorithm
    options = optimset('Display','off','MaxIter',100000,'TolFun',1e-10,'TolX',1e-10,...
        'DiffMaxChange',1e-2,'DiffMinChange',1e-4,'MaxFunEvals',10000,'LargeScale','off');
    
    % Define the objective function handle
    objFunHandle = @(eta) KFL(eta, data_sub);

    % Call fmincon to minimize the negative log-likelihood
    [etaOpt, negLogLikelihoodOpt] = fmincon(objFunHandle, initialEta, [], [], [], [], lb, ub, [], options);

    % Display results
    disp(['Optimal eta: ', num2str(etaOpt)]);
    etalist(n) = etaOpt;
    % data(data.ID == n, :) = data_sub;
end

% cd("output/")
% save("estimated_data.mat","data")
% writetable(data,"data.csv")
% cd(workpath)