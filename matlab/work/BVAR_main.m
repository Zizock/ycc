clear all, close all

% the main script for my BVAR model
% two versions: benchmark / robustness check, based on settings
% structural shocks identified with sign restrictions
% method based on: Jarocinsky and Karadi (2020)

% Parts:
% 1, set up mt and yt; link with their full names
% 2, prepare priors for Bayesian estimation; import data
% 3, call BAVR function for estimation
% 4, recover structural shocks via sign restrictions
% 5, plot irfs
% 6, export mps and info shocks and save
% end: some local functions


%% define date range
spl = [2001 3; 2022 11];

id_method = 'signrestriction';
% PURPOSE: this is solely for generating an output file name in the function
% graph_irfs


%=======================================================================
%% Set up mt
%=======================================================================

% PURPOSE: define mt series (high frequency part)
% pick one by uncommenting:

%mnames = {'d_euroyen1','d_N225'};
%mnames = {'d_euroyen3','d_N225'};
%mnames = {'d_JGB1','d_N225'};
%mnames = {'d_JGB2','d_N225'};
%mnames = {'d_JGB5','d_N225'};
mnames = {'d_JGB10','d_N225'};
%mnames = {'target_factor','d_N225'};
%mnames = {'path_factor','d_N225'};

%mnames = {'g_euroyen1','g_N225'};
%mnames = {'g_euroyen3','g_N225'};
%mnames = {'g_JGB1','g_N225'};
%mnames = {'g_JGB2','g_N225'};
%mnames = {'g_JGB5','g_N225'};
%mnames = {'g_JGB10','g_N225'};
%mnames = {'g_target_factor','g_N225'};
%mnames = {'g_path_factor','g_N225'};

%mnames = {'lg_euroyen1','lg_N225'};
%mnames = {'lg_euroyen3','lg_N225'};
%mnames = {'lg_JGB1','lg_N225'};
%mnames = {'lg_JGB2','lg_N225'};
%mnames = {'lg_JGB5','lg_N225'};
%mnames = {'lg_JGB10','lg_N225'};
%mnames = {'lg_target_factor','lg_N225'};
%mnames = {'lg_path_factor','lg_N225'};

%mnames = {'d_JGB20','g_N225'};
%mnames = {'d_TSR5','d_N225'};
%mnames = {'d_TSR10','d_N225'};
%mnames = {'d_CB1','d_N225'};
%mnames = {'d_CB10','d_N225'};


%mnames = {'g_JGB20','g_N225'};
%mnames = {'g_TSR5','g_N225'};
%mnames = {'g_TSR10','g_N225'};
%mnames = {'g_CB1','g_N225'};
%mnames = {'g_CB10','g_N225'};


% PURPOSE: give full names (in string) to mt series
mdict = {
    'd_euroyen1', 'surprise in \newline3mo future';
    'd_euroyen3', 'surprise in \newline9mo future';
    'd_JGB1', 'surprise in \newline1year gov bond';
    'd_JGB2', 'surprise in \newline2year gov bond';
    'd_JGB5', 'surprise in \newline5year gov bond';
    'd_JGB10', 'surprise in \newline10year gov bond';
    'd_N225', 'surprise in \newlineNikkei 225 index';
    'target_factor', 'factor analysis \newlineTarget Factor';
    'path_factor', 'factor analysis \newlinePath Factor';

    'g_euroyen1', 'surprise in \newline3mo future';
    'g_euroyen3', 'surprise in \newline9mo future';
    'g_JGB1', 'surprise in \newline1year gov bond';
    'g_JGB2', 'surprise in \newline2year gov bond';
    'g_JGB5', 'surprise in \newline5year gov bond';
    'g_JGB10', 'surprise in \newline10year gov bond';
    'g_N225', 'surprise in \newlineNikkei 225 index';
    'g_target_factor', 'factor analysis \newlineTarget Factor';
    'g_path_factor', 'factor analysis \newlinePath Factor';

    'lg_euroyen1', 'surprise in \newline3mo future';
    'lg_euroyen3', 'surprise in \newline9mo future';
    'lg_JGB1', 'surprise in \newline1year gov bond';
    'lg_JGB2', 'surprise in \newline2year gov bond';
    'lg_JGB5', 'surprise in \newline5year gov bond';
    'lg_JGB10', 'surprise in \newline10year gov bond';
    'lg_N225', 'surprise in \newlineNikkei 225 index';
    'lg_target_factor', 'factor analysis \newlineTarget Factor';
    'lg_path_factor', 'factor analysis \newlinePath Factor';

    'd_JGB20', 'surprise in \newline20year gov bond';
    'd_TSR5', 'surprise in \newlineTokyo 5yr swap';
    'd_TSR10', 'surprise in \newlineTokyo 10yr swap';
    'd_CB1', 'surprise in \newlineCorporate bond yield';
    'd_CB10', 'surprise in \newlineCorporate bond yield';
    
    'g_JGB20', 'surprise in \newline20year gov bond';
    'g_TSR5', 'surprise in \newlineTokyo 5yr swap';
    'g_TSR10', 'surprise in \newlineTokyo 10yr swap';
    'g_CB1', 'surprise in \newlineCorporate bond yield';
    'g_CB10', 'surprise in \newlineCorporate bond yield'
    };
% mdict can extend very long to include alternative model variables

% PURPOSE: match mnames with variables in mdict, pick up the correct
% full names for those mt variables used
mnames_nice = applydictregexp(mnames, mdict); 

mylimits = nan(length(mnames),2);

%=======================================================================
%% Set up yt
%=======================================================================
% PURPOSE: define yt series (low frequency variables)
ny1 = 5;

% pick one by uncommenting:
ynames = {'JGB1','N225','md_IP','md_CPI','tp5'};
%ynames = {'JGB2','N225','md_IP','md_CPI','tp5'};
%ynames = {'JGB5','N225','md_IP','md_CPI','tp5'};

% full names of yt series, limits, etc... info stored in yditc.csv
% ydict only needs to contian low frequency variables in yt
dictfname = '../data_var/ydict.csv';
fileID = fopen(dictfname);
% read yt disctionary
ydict = textscan(fileID,'%s %q %d %f %f','Delimiter',',','HeaderLines',1);
fclose(fileID);

% PURPOSE: match yt variables with their full names
ynames_nice = applydict(ynames, [ydict{1} ydict{2}]);%ynames_nice = ynames;
yylimits = [ydict{4} ydict{5}];
yylimits = yylimits(findstrings(ynames,ydict{1}),:);
nonst = ydict{3};
nonst = nonst(findstrings(ynames,ydict{1}),:); % this line is added later

%=======================================================================
%% More preparations: prior, output, etc...
%=======================================================================

% define prior settings
% Minnesota prior requires st/nonst variables, which should be read from
% data below
prior.lags = 12;
prior.minnesota.tightness = 0.2;
prior.minnesota.decay = 1;
prior.Nm = length(mnames);


% generate output folder
st = dbstack; 
pathout = st(end).name; 
clear st

mkdir(pathout)
pathout = [pathout '/'];


% functions for time operations
ym2t = @(x) x(1)+x(2)/12 - 1/24; % convert [year month] into time
t2datestr = @(t) [num2str(floor(t)) char(repmat(109,length(t),1)) num2str(12*(t-floor(t)+1/24),'%02.0f')];
t2ym = @(t) [floor(t) round(12*(t-floor(t)+1/24))]; % convert time into [year month]
ymdif = @(x1,x2) (x2(1)-x1(1))*12+x2(2)-x1(2);
findym = @(x,t) find(abs(t-ym2t(x))<1e-6); % find [year month] in time vector t

% Gibbs sampler settings
gssettings.ndraws = 4000;
gssettings.burnin = 4000;
gssettings.saveevery = 4;
gssettings.computemarglik = 0;


%=======================================================================
%% load data and read the variables needed
%=======================================================================
% set one from below:

datafname = '../data_var/yt_select.csv';
%datafname = '../data_var/yt_select_extramatlab.csv';
%datafname = '../data_var/yt_select_extra.csv';

data.Nm = length(mnames);
data.names = [mnames ynames];
d = importdata(datafname);
dat = d.data;
txt = d.colheaders;
tbeg = find(dat(:,1)==spl(1,1) & dat(:,2)==spl(1,2)); 
if isempty(tbeg), 
    tbeg=1;
end
tend = find(dat(:,1)==spl(2,1) & dat(:,2)==spl(2,2));
if isempty(tend), 
    tend=size(dat,1);
end

% PURPOSE: find the position of data.name in txt()
ysel = findstrings(data.names, txt(1,:));
data.y = dat(tbeg:tend, ysel);
data.w = ones(size(data.y,1),1);
% use linspace() to generate evenly distributed dots as time periods 
% in the data
data.time = linspace(ym2t(dat(tbeg,1:2)), ym2t(dat(tend,1:2)), size(data.y,1))';
clear d dat txt tbeg tend ysel

% complete the prior
prior.minnesota.mvector = [zeros(data.Nm,1); nonst];

% output file names
fname = [pathout 'japan_output'];
diary([fname '.txt'])
data = checkdata(data, t2datestr, 1:data.Nm); % again, for the diary
plot_y;

% replace NaNs with zeros in the initial condition
temp = data.y(1:prior.lags,:);
temp(isnan(temp)) = 0;
data.y(1:prior.lags,:) = temp; 

%=======================================================================
%% estimate the VAR (use another .m file, by Gibbs MCMC)
%=======================================================================
% pick one from below: benchmark model / robustness check

% call the benchmark version of VAR model
res = var_benchmark(data, prior, gssettings);

% call the robustness check version of VAR model
%res = var_robustness_check(data, prior, gssettings);

% save the data series used in the VAR estimation
% make it easier to double check
savedata([fname '_data.csv'], data, t2ym)

%=======================================================================
%% identify the shocks, and calculate(draw) IRFs to both shocks
%=======================================================================

MAlags = 36;
N = length(data.names);

shocknames = [{'mon.pol.', 'CBinfo'} mnames(2+1:end) ynames];
dims = {[1 2]};
imonpol = 1; inews = 2;
test_restr = @(irfs)... %% restrictions by shock (i.e. by column):
    irfs(1,imonpol,1) > 0 && irfs(2,imonpol,1) < 0 &&... % mp
    irfs(1,inews,1) > 0 && irfs(2,inews,1) > 0; % info

% copy one to above function and uncomment
% for yield data
    %irfs(1,imonpol,1) > 0 && irfs(2,imonpol,1) < 0 &&... % mp
    %irfs(1,inews,1) > 0 && irfs(2,inews,1) > 0; % info
% for future index data
    %irfs(1,imonpol,1) < 0 && irfs(2,imonpol,1) < 0 &&... % mp
    %irfs(1,inews,1) < 0 && irfs(2,inews,1) > 0; % info

b_normalize = ones(1,N);
max_try = 1000;
global C_saved;
global Q_saved;
% save the results from external files and use them to export mps and info
disp(test_restr)
irfs_draws = sign_restriction_draw(res, MAlags, dims, test_restr, b_normalize, max_try);
% call an external function file
% PURPOSE: calculate IRFs, store the irf draws

%[irfs_draws, irfs_l_draws, irfs_u_draws] = sign_restriction_draw_robust(res, MAlags, dims, test_restr, b_normalize, max_try);

%=======================================================================
%% report results and plot irfs
%=======================================================================
ss = 1:2;

% 1, variance decomposition
vdec_mean = table_vdecomp(irfs_draws, 1:N, ss, data.names, shocknames, 24);
% call external function file
% purpose: var decomposition

% 2, report irfs
qtoplot = [0.5 0.16 0.84 0.05 0.95]; % quantiles to plot
varnames = [mnames, ynames]; varnames_nice = [mnames_nice ynames_nice]; shocknames_nice = shocknames;
ylimits = [];
%ylimits = [mylimits; yylimits];
transf = nan(N,2);

% 3, print out the impact responses
table_irf(irfs_draws, ss, 1, varnames, qtoplot(1:3));
table_irf(irfs_draws, ss, 1, varnames, qtoplot([1 4 5]));
% call external function file
% PURPOSE: print tables of results

% 4, plot irfs
hh = graph_irfs(irfs_draws, data.Nm+1:min(N,data.Nm+ny1), ss, varnames_nice, varnames, shocknames_nice, id_method, qtoplot, [0 0 1], '', ylimits, transf); align_Ylabels(hh); saveTightFigure(hh,[fname '_irfy1'],'pdf')

if 0 % save the ylimits
    hh = graph_irfs(irfs_draws, 1:N, ss, varnames_nice, varnames, shocknames_nice, id_method, qtoplot, [0 0 1], '', ylimits, transf); align_Ylabels(hh);
    ylimits = cell2mat(get(hh.Children,'Ylim')); ylimits = ylimits(1:max(ss):N*max(ss),:); ylimits = flipud(ylimits);
    save([fname '_ylimits.mat'],'ylimits');
end


%=======================================================================
%% last: generate mps and info series and export to csv file
%=======================================================================
% get original columns of shocks from yt
extracted_y = data.y(:,1:2);

% capture the rotation matrix between reduced shocks and structural shocks
CQ_saved = nan(7,7,4000);
inversed_CQ_saved = nan(7,7,4000);
block_2X2 = nan(2,2,4000);
sum_block = zeros(2,2);

results = zeros(2,length(extracted_y));

for i =1:4000
    CQ_saved(:,:,i) = C_saved(:,:,i)'*Q_saved(:,:,i);
    inversed_CQ_saved(:,:,i) = inv(CQ_saved(:,:,i));
end

% apply the matrix to all draws of shocks
for i = 1:4000
    block_2X2(:,:,i) = inversed_CQ_saved(1:2,1:2,i);
    mpandinfo = block_2X2(:,:,i) * extracted_y(:,:)';
    results = results + mpandinfo;
    sum_block = sum_block(:,:) + block_2X2(:,:,i);
end

% average over all draws
average_shocks = results/4000;

avg_block = sum_block/4000;

final_shocks = average_shocks';

scaled_shocks = final_shocks;

% scale the mps and info by the size of HF MP indicator
AA = avg_block(1,1);
BB = avg_block(1,2);
CC = avg_block(2,1);
DD = avg_block(2,2);

scaled_shocks(:,1) = final_shocks(:,1)*DD/(AA*DD-CC*BB);
scaled_shocks(:,2) = final_shocks(:,2)*(-BB)/(AA*DD-BB*CC);


shockratio = final_shocks(:,1) ./ final_shocks(:,2);

filename = 'shocks_output.csv';

writematrix(scaled_shocks, filename);


%=======================================================================
%% short local functions
%=======================================================================
% Some functions below belong to Jarocinski and Karadi (2020)

function outarray = applydictregexp(inarray, mydict)
outarray = inarray;
for n = 1:length(outarray)
    count = 0;
    for i = 1:size(mydict, 1)
        startIndex = regexp(outarray{n}, mydict{i, 1});
        if ~isempty(startIndex)
            outarray(n) = mydict(i, 2);
            count = count + 1;
        end
    end
    if count > 1
        error('multiple matches found!');
    end
end
end

function outarray = applydict(inarray, mydict)
outarray = inarray;
for n = 1:length(outarray)
    i = find(strcmp(outarray(n),mydict(:,1)));
    if ~isempty(i)
        outarray(n) = mydict(i,2);
    end
end
end

function idx = findstrings(astr1, astr2)
if ischar(astr1)
    astr1 = {astr1};
end

idx = nan(1,length(astr1));
for i = 1:length(astr1)
    if isempty(find(strcmp(astr2, astr1{i})))
        error(['string ' astr1{i} ' not found'])
    else
        idx(i) = find(strcmp(astr2, astr1{i}));
    end
end
end

























