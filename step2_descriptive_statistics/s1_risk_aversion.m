%% fit risk aversion parameters gamma
% Mehra and Prescott (1985) suggest that γ = 10 is an upper bound for a reasonable value of relative risk aversion.
% Reference: 
%   Mehra, R., & Prescott, E. C. (1985). The equity premium: A puzzle. Journal of Monetary Economics, 15(2), 145–161. https://doi.org/10.1016/0304-3932(85)90061-3
%   Boyle, P. A., Yu, L., Buchman, A., & Bennett, D. (2012). Risk aversion is associated with decision making among community-based older persons. Frontiers in Psychology, 3. https://doi.org/10.3389/fpsyg.2012.00205

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
    gamma = fminbnd(fun, 0, 10);
    ra_data.gamma(ra_data.ID == id) = gamma;
end

% save data
writetable(ra_data, "output/risk_aversion.csv")