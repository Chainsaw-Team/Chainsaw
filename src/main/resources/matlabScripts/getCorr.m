function corrFactor = getCorr(yours, golden)

convresult = conv(flip(yours),golden);
[~, start] = max(convresult);
start = start - length(golden) + 1;

if start > 0
    g = golden(start: end);
    corrLength = length(g);
    y = yours(1: corrLength);
else
    y = yours(-start + 2: end);
    corrLength = length(y);
    g = golden(1: corrLength);
end

plot(y, 'y')
title('yours')
hold on
plot(g, 'g')
title('golden')
saveas(gcf, 'corr.png')

corrMatrix = corrcoef(y, g);
corrFactor = corrMatrix(1,2); % 相关系数