close all;
clear all;

%% read in data file
fileToRead1 = 'CR2111_M-MOQ_Matrix_SpeedBand4.dat';
plotTitle = '300 km/s < V_{sw} < 400 km/s';

rawData1 = importdata(fileToRead1);

% For some simple files (such as a CSV or JPEG files), IMPORTDATA might
% return a simple array.  If so, generate a structure so that the output
% matches that from the Import Wizard.
[unused,name] = fileparts(fileToRead1); %#ok
newData1.(genvarname(name)) = rawData1;

% Create new variables in the base workspace from those fields.
vars = fieldnames(newData1);
for i = 1:length(vars)
    assignin('base', vars{i}, newData1.(vars{i}));
end

%% define box
moqO7 = [2.11,2.46];
massO7 = [14.21,20.41];
moqO6 = [2.54,2.87];
massO6 = [10.83,20.41];

moqC6 = [1.87,2.18];
massC6 = [9.04,12.99];
moqC5 = [2.25,2.54];
massC5 = [9.04,12.99];
moqC4 = [2.87,3.24];
massC4 = [9.04,12.99];

MOQMMat = rawData1;

%% get data ready
km = (95.0/0.5)^(1/58);
massRange = zeros(59,1);
for i=2:59
    massRange(i) = 0.5*km^(i-2);
end

kq = (42/0.9)^(1/126);
moqRange = zeros(127,1);
for i=2:127
    moqRange(i) = 0.9*kq^(i-2);
end

lenData = length(MOQMMat(:,1));
countsPerMoq = zeros(127,1);
countsMatrix = zeros(59,127);
for i=1:lenData
    nqBin = MOQMMat(i,2)+1;
    nmBin = MOQMMat(i,1)+1;
    countsPerMoq(nqBin) = countsPerMoq(nqBin)+1;
    countsMatrix(nmBin,nqBin) = countsMatrix(nmBin,nqBin)+1;
end

indZero = find(countsMatrix == 0);
countsMatrix(indZero) = NaN;

%% make plots

%figure(1);
subplot(4,1,[1 2 3]);
%contourf(moqRange,massRange,log10(countsMatrix));
pcolor(moqRange(1:127),massRange(1:59),countsMatrix);
shading flat;
axis([0.5 42.0 0.1 100.0]);
set(gca,'YScale','log');
set(gca,'ytick',[0.1, 1.0, 10.0, 100.0]);
set(gca,'yticklabel',{'  0.1';'  1.0';' 10.0';'100.0'});
set(gca,'XScale','log');
set(gca,'xticklabel',{''});
set(gca,'FontSize',14,'FontWeight','Bold','LineWidth',2,'TickDir','out')
title(plotTitle,'FontWeight','bold');
ylabel('MASS (amu)');
rectangle('position',[moqO7(1),massO7(1),(moqO7(2)-moqO7(1)),(massO7(2)-massO7(1))],'linewidth',2,'linestyle','--','edgecolor','red')
rectangle('position',[moqO6(1),massO6(1),(moqO6(2)-moqO6(1)),(massO6(2)-massO6(1))],'linewidth',2,'linestyle','--','edgecolor','green')
rectangle('position',[moqC6(1),massC6(1),(moqC6(2)-moqC6(1)),(massC6(2)-massC6(1))],'linewidth',2,'linestyle','--','edgecolor','red')
rectangle('position',[moqC5(1),massC5(1),(moqC5(2)-moqC5(1)),(massC5(2)-massC5(1))],'linewidth',2,'linestyle','--','edgecolor','green')
rectangle('position',[moqC4(1),massC4(1),(moqC4(2)-moqC4(1)),(massC4(2)-massC4(1))],'linewidth',2,'linestyle','--','edgecolor','yellow')

subplot(4,1,4);
plot(moqRange,countsPerMoq, '--bs');
axis([0.5 42.0 0.1 100000.0]);
set(gca,'YScale','log');
set(gca,'XScale','log');
set(gca,'ytick',[10^0,10^1,10^2,10^3,10^4,10^5]);
set(gca,'xtick',[1.0,10.0]);
set(gca,'xticklabel',{' 1';'10'});
set(gca,'FontSize',14,'FontWeight','Bold','LineWidth',2,'TickDir','out')
ylabel('Counts');
xlabel('MASS/CHARGE (amu/e)');

%% make with scatter plot
% figure(2);
% subplot(4,1,[1 2 3]);
% scatter(MOQMMat(:,2),MOQMMat(:,1),'x');
% 
% axis([0.5 42.0 0.1 100.0]);
% axis([0 127 0 59]);
% set(gca,'YScale','log');
% set(gca,'ytick',[0.1, 1.0, 10.0, 100.0]);
% set(gca,'yticklabel',{'  0.1';'  1.0';' 10.0';'100.0'});
% set(gca,'XScale','log');
% set(gca,'xticklabel',{''});
% set(gca,'FontSize',14,'FontWeight','Bold','LineWidth',2,'TickDir','out')
% ylabel('MASS (digital bin)');
% rectangle('position',[moqMin,massMin,moqWidth,massHeight],'linewidth',2,'linestyle','--','edgecolor','red')
% 
% subplot(4,1,4);
% plot(1:127,countsPerMoq, '--bs');
% axis([0.5 42.0 0.1 100000.0]);
% axis([0 127 0.1 100000.0 ]);
% set(gca,'YScale','log');
% set(gca,'XScale','log');
% set(gca,'ytick',[10^0,10^1,10^2,10^3,10^4,10^5]);
% set(gca,'xtick',[1.0,10.0]);
% set(gca,'xticklabel',{' 1';'10'});
% set(gca,'FontSize',14,'FontWeight','Bold','LineWidth',2,'TickDir','out')
% ylabel('Counts');
% xlabel('MASS/CHARGE (digital bin)');