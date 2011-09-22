clear
f = 'MuscleSet1.txt';
data = dlmread(f,' ');
x = data(:,1);
y = data(:,2);
ok = (0 <= y) & (y <= y(1));
x = x(ok);
y = y(ok);
plot(x,y)
legend('Time Series Curve for Decay of Oxygen Concentration: Muscle Set 1');
xlabel('Time [sec]', 'FontSize', 12);
ylabel('Concentration [mM]', 'FontSize', 12);
