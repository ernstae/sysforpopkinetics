% function MuscleO2Uptake
% Matlab script to integrate the PDE:
%   dCm/dt = D[(1/r) (d/dr) r (d/dr) ] Cm - Fmr(1 + Fms(t))/Vm
% for a cylindrical tissue (muscle) model, with the initial 
% and boundary conditions as
%   Cm(r,t=0) = Cm0,
%   [dCm/dt(r=Rm,t)] = -Fel/Vch - (2*pi*Rm*Lm/Vch)*[D*dCm/dr(r=R,t)]
%   dCm/dr(r=0,t) = 0

% Read the columns from data file based on the Apparatus 
% Baseline experiment
clear
omatrix ReadCalibration.oms
%
file = 'MuscleSet1.txt';
[col1, col2] = ReadCalibration(file);
% col1 = Time in seconds
% col2 = Amount of O2 in nmoles
Ndata = length(col1);	% Number of data points
Tfdata = col1(Ndata);	% Final time in data in second
qch0 = 1119.42;		% Initial amount of O2 in the chamber 
			% qch0 = col2(1), nmoles

% Define the model parameters obtained from SAAM II or otherwise
Tf = Tfdata;            % final time in seconds
Rm = 0.05;              % radius of muscle in centimeter
Lm = 1;                 % length of muscle in centimeter
D = 1e-5;               % diffusion coefficient in cm^2/sec
Vch = 5.4;              % Volume of the chamber, mL
Vm = pi*Rm^2*Lm;        % Volume of the muscle, mL
Ams = 2*pi*Rm*Lm;       % Area of the muscle surface, cm^2
alphaO2 = 0.347;        % Solubility of O2, nmoles/(mL*mmHg)
Fel = 0.0125;           % Electrode consumption rate, nmoles/sec
Fmr = 0.00048;          % Resting muscle O2 consumption rate, nmoles/sec 

% Define the numerical parameters for simulation
Nr = 11;		% number of radial points
dr = Rm/(Nr-1);		% step length in radial direction
dt = dr^2/(10*D);	% step length in time, using stability criteria
Nt = round(1+Tf/dt);    % number of temporal points
fact = D*dt/dr^2;	% a multiplicative factor in the numerical scheme
t = [0:dt:Tf];		% array (vector) of temporal grid points
r = [0:dr:Rm];		% array (vector) of radial grid points

% Forcing function (Fm) and the initial concentration in the muscle (Cm0)
for j = 1:Nt,           % Input function for muscle stimulation
    if (t(j) >= 800 & t(j) <= 1475)
       Fms(j) = 40;
    else
       Fms(j) = 0;
    end
end
Fm = Fmr*(1+Fms);
Cch0 = qch0/Vch;	% initial concentration in the chamber/muscle surface
for i = 1:Nr,           % initial concentration in the muscle
    Cm0(i) = Cch0 - (Fm(1)/(4*Vm*D))*(Rm^2-r(i)^2);   % psudo-steady state
end
    
% Computation of Cm using explicit (FTCS) finite difference (FD) scheme
coeff1(1) = 0;
coeff2(1) = 0;
for i = 2:Nr,
    coeff1(i) = 1-dr/(2*r(i));  % coefficient 1 in the FD scheme
    coeff2(i) = 1+dr/(2*r(i));  % coefficient 2 in the FD scheme
end
Cm = zeros(Nr,Nt);	% concentration grid to start with
Cm(:,1) = Cm0(:);	% initial concentration in the domain
for j = 2:Nt,		% concentration at various time levels
    Cm(1,j) = Cm(1,j-1) + 2*fact*(Cm(2,j-1)-Cm(1,j-1)) - dt*Fm(j-1)/Vm;
    for i = 2:Nr-1,
        Cm(i,j) = Cm(i,j-1) + fact*(coeff1(i)*Cm(i-1,j-1) - 2*Cm(i,j-1) ...
	            + coeff2(i)*Cm(i+1,j-1)) - dt*Fm(j-1)/Vm;
    end
    Cm(Nr,j) = Cm(Nr,j-1) - (dt*Fel/Vch) - (Ams*fact*dr/Vch) ...
             * (Cm(Nr,j-1) - Cm(Nr-1,j-1));
end
pO2m = Cm/alphaO2;
Cch = Cm(Nr,:);
qch = Vch*Cch;
pO2ch = Cch/alphaO2;

% Compute the O2 uptake at the muscle surface
for j = 1:Nt,
    O2Uptake(j) = (2*D*Vm/(Rm*dr))*(Cm(Nr,j)-Cm(Nr-1,j));
end

% Plot the model fit to the experimental data in figure 2 and 
% other solutions in figure 1, figure 3 and figure 4.
omgaddwin('Figure 1');
title('Stimulation function Fms(t) and consumption rates Fel & Fm(t)');
omgaddview(.2, .5, .8, .5); % subplot(211)
plot(t, Fms, '-r');
xlabel('Time [sec]');
ylabel('Fms(t) [unitless]');
%
% plot(t, Fm, '-r', t, Fel, '-b', t, O2Uptake, '-b');
%  legend('Fm', 'Fel', 'O2Uptake', 1);
omgaddview(.2, 0, .8, .5) % subplot(212)
color = ['red'; 'blue'; 'green']
omgcolor( color );
omgplot(t', Fm', 'solid');
omgplot( [t(1); t(end)], [Fel; Fel], 'solid' );
omgplot(t', O2Uptake', 'solid');
legend = [ 'Fm  red'; 'Fel blue'; 'O2  green'];
omgaddtext( legend, [0, .5], [0, 1.]);
xlabel('Time [sec]');
ylabel('Consumption rates [nmoles/sec]');
%
omgaddwin('Figure 2');
omgaddview(.2, 0, .8, 1.)
title('Decay of oxygen in the chamber with muscle stimulated');
omgcolor( color );
omgplot(col1, col2, 'solid');
omgplot(t', qch', 'solid');
legend = [ 'Data red'; 'Fit  blue' ];
omgaddtext( legend, [0, .75], [0, 1.]);
xlabel('Time [sec]');
ylabel('Amount [nmoles]');
%
omgaddwin('Figure 3');
title('Concentration and partial pressure of O2 in the chamber');
omgaddview(.2, .5, .8, .5); % subplot(211)
plot(t, Cch, '.r')
xlabel('Time [sec]');
ylabel('Concentration [nMolar]');
%
omgaddview(.2, 0, .8, .5); % subplot(212)
plot(t, pO2ch, '.b');
xlabel('Time [sec]');
ylabel('pO2 [mmHg]');
%
omgaddwin('Figure 4');
title('Concentration and partial pressure of O2 in the Muscle');
omgaddview(.2, .5, .8, .5); % subplot(211)
omgcolor('red');
omgplot(t', Cm', 'solid');
xlabel('Time [sec]');
ylabel('Concentration [nMolar]');
%
omgaddview(.2, 0, .8, .5); % subplot(212)
omgplot(t', pO2m', 'solid');
xlabel('Time [sec]');
ylabel('pO2 [mmHg]');
