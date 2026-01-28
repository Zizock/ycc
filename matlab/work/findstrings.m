% this script belongs to Jarocinski and Karadi (2020)

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
