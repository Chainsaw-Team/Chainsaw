function corrFactor = getCorr(yours, golden)

l = min(length(yours), length(golden));
golden = golden(1:l);
yours = yours(1:l);

%yours = (yours - mean(yours));
%golden = (golden - mean(golden));

figure()

plot(yours, 'y')
hold on
plot(golden, 'g')
legend('yours', 'golden')
saveas(gcf, '/home/ltr/Chainsaw/src/main/resources/matlabGenerated/raw.png')

% lag value shold be within 1000 elements
%[value, lags] = xcorr(golden(1:1000), yours(1:1000));
corrLength = min(length(yours), 200);
[value, lags] = xcorr(golden(1:corrLength), yours(1:corrLength));
[~,I] = max(value);
lag = lags(I);

g = circshift(golden, -lag);
y = yours;

%g = g(abs(lag): end - abs(lag));
%y = y(abs(lag): end - abs(lag));

disp(['your latency is larger than true latency by ', num2str(lag), ' elements'])
corrMatrix = corrcoef(y, g);
corrFactor = corrMatrix(1,2); % 相关系数

%if corrFactor < 0.9

figure()

plot(y, 'y')
hold on
plot(g, 'g')
legend('yours', 'golden')
saveas(gcf, '/home/ltr/Chainsaw/src/main/resources/matlabGenerated/corr.png')

figure()

window = boxcar(length(g));
[p0, f0] = periodogram(y, window, 512, 250e6);
[p1, f1] = periodogram(g, window, 512, 250e6);
plot(f0, 10*log10(p0 / 2), 'y')
hold on
plot(f1, 10*log10(p1 / 2), 'g')
legend('yours', 'golden')
saveas(gcf, '/home/ltr/Chainsaw/src/main/resources/matlabGenerated/spectrum.png')

%end