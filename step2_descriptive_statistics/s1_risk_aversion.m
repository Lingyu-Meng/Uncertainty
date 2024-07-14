%% fit risk aversion parameters gamma

% load data
wd = pwd;
cd("../")
ra_data = readtable("step1_data_wrangling/output/RAS.csv");
cd(wd)

% Initialize gamma column
ra_data.gamma = zeros(height(ra_data), 1);

%% model function (Log likelihood function)
% Gamma: gamma parameter
% Gain: input gain of gamble
% Safe: input value of safe
% response: input response of participant
function LL = log_likelihood(Gamma, Gain, Safe, Response)
    % Compute expected utilities
    U_g = 0.5 * Gain.^(1 - Gamma) ./ (1 - Gamma); % Expected utility of gamble
    U_s = Safe.^(1 - Gamma) ./ (1 - Gamma);       % Expected utility of safe
    
    % Compute odds ratio and probability of choosing gamble
    oddsratio = exp(U_g - U_s);
    p_gamble = oddsratio ./ (1 + oddsratio);
    
    % Compute log likelihood
    LL = Response .* log(p_gamble) + (1 - Response) .* log(1 - p_gamble);
    LL = -sum(LL); % Return negative sum of log likelihood for minimization
end

% Fit risk aversion parameters
unique_ids = unique(ra_data.ID);
for i = 1:length(unique_ids)
    id = unique_ids(i);
    ra_data_i = ra_data(ra_data.ID == id, :);
    
    % Define the function to minimize
    fun = @(x) log_likelihood(x, ra_data_i.gain, ra_data_i.safe, ra_data_i.Response);
    
    % Fit gamma using fminbnd
    gamma = fminbnd(fun, 0, 1);
    ra_data.gamma(ra_data.ID == id) = gamma;
end

% save data
writetable(ra_data, "output/risk_aversion.csv")