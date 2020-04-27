%% Clean Up
close all;
clear all;

%% Read in file

fileToRead1 = '2004.dat';

DELIMITER = ' ';
HEADERLINES = 3;

% Import the fileset(gca,'Fontsize',22,'Fontweight','bold','linewidth',2);
newData1 = importdata(fileToRead1, DELIMITER, HEADERLINES);

% Create new variables in the base workspace from those fields.
vars = fieldnames(newData1);
for i = 1:length(vars)
    assignin('base', vars{i}, newData1.(vars{i}));
end


%% Make Plots

index = find(data == 0.0);
data(index) = nan;
dataPlot = log10(data);

dvs = 0:31;
tofCh = 0:1023;
tofs = (tofCh-44)/2.372530695;
eoq = 6.190722.*(1.1225857).^dvs;
deltaEoq = 0.019.*eoq;

%Calculate the Energy Loss in Foil Curve
%Neon
a(1) = -0.02702;
a(2) = -0.02900;
a(3) =  0.01800;
a(4) = -0.00033;
a(5) = -0.00528;
a(6) = -0.00380;
b(1) =  0.43535;
b(2) = -0.30235;
b(3) =  0.11514;
b(4) = -0.05078;
b(5) =  0.02614;
b(6) = -0.01067;
a0   =  0.92727;
w	 =  0.41196;
cThick = 2.0;
eloss = ones(1,32)*a0;
for i = 1:32
	for j = 1:6
		eloss(i) = eloss(i)+a(j)*cos(j*log10(eoq(i))*w)+b(j)*sin(j*log10(eoq(i))*w);
	end
end

%oxygen
% a(1) = -0.11413;
% a(2) =  0.05504;
% a(3) = -0.03512;
% a(4) =  0.02889;
% a(5) = -0.02268;
% a(6) =  0.01056;
% b(1) =  0.41147;
% b(2) = -0.25387;
% b(3) =  0.08634;
% b(4) = -0.01926;
% b(5) =  0.00010;
% b(6) = -0.00048;
% a0   =  0.39708;
% w	 =  0.96021;
% cThick = 2.0;
% eloss = ones(1,32)*a0;
% for i = 1:32
% 	for j = 1:6
% 		eloss(i) = eloss(i)+a(j)*cos(j*log10(eoq(i))*w)+b(j)*sin(j*log10(eoq(i))*w);
% 	end
% end

% %hydrogen
% a(1) = -1.29068;
% a(2) =  0.61403;
% a(3) = -0.05186;
% a(4) =  1.90217;
% a(5) = -0.06770;
% a(6) = -3.46247;
% b(1) = -1.92264;
% b(2) =  2.29322;
% b(3) =  0.12056;
% b(4) =  3.99194;
% b(5) =  2.59102;
% b(6) =  4.37465;
% c(1) =  2.53054;
% c(2) =  1.11198;
% c(3) =  0.85214;
% c(4) =  1.42891;
% c(5) =  0.50814;
% c(6) =  2.00139;
% cThick = 2.0;
% eloss = zeros(1,32);
% for i = 1:32
%     for j = 1:6
%         eloss(i) = eloss(i) + a(j)*exp((-(log10(eoq(i))-b(j))/c(j))^2);
%     end
% end

for i = 1:32
    eloss(i) = 10^eloss(i)*cThick;
end

%determine ion physics trajectory
mass = 20.180;
charge = 1;
distance = 10/100;  %meters
eTOT = charge.*(eoq)-eloss;
vion = 437.74.*sqrt(eTOT./mass).*1000;
tOffset = 3;
time = (distance./vion);
time = time.*(10^9)+tOffset;  %convert to ns
TCHion = 2.372530695*time+44;

% calculate TOF Ranges
% tofHigh = (2.372530695.*((distance./(437.74.*sqrt((charge.*(eoq-deltaEoq)-eloss)./mass).*1000)).*(10^9))+44)*1.15;
% tofLow  = (2.372530695.*((distance./(437.74.*sqrt((charge.*(eoq+deltaEoq)-eloss)./mass).*1000)).*(10^9))+44)*0.95;
tofHigh = zeros(32,1);
tofLow = zeros(32,1);
 for i = 1:32
%     if (i < 17)
%         tofHigh(i) = (2.372530695.*((distance./(437.74.*sqrt((charge.*(eoq(i)-deltaEoq(i))-eloss(i))./mass).*1000)).*(10^9))+44)*1.25;
%         tofLow(i)  = (2.372530695.*((distance./(437.74.*sqrt((charge.*(eoq(i)+deltaEoq(i))-eloss(i))./mass).*1000)).*(10^9))+44)*1.05;
%     else
%         velPartHigh = (437.74.*sqrt((charge.*(eoq(i)-deltaEoq(i)-eloss(i)))./mass).*1000).*10^9;
%         velPartLow  = (437.74.*sqrt((charge.*(eoq(i)+deltaEoq(i)-eloss(i)))./mass).*1000).*10^9;
%         timeHigh(i) = distance./velPartHigh;
%         timeLow(i)  = distance./velPartLow;
%         tofHigh(i) = (2.372530695.*(timeHigh(i)+44))*1.05;
%         tofLow(i)  = (2.372530695.*(timeLow(i)+44))*0.98;
        if (imag(vion(i)) == 0.0)
            tofHigh(i) = real((2.372530695.*((distance./(437.74.*sqrt((charge.*(eoq(i)-deltaEoq(i))-eloss(i))./mass).*1000)).*(10^9))+44))*1.07;
            tofLow(i)  = real((2.372530695.*((distance./(437.74.*sqrt((charge.*(eoq(i)+deltaEoq(i))-eloss(i))./mass).*1000)).*(10^9))+44))*.95;
        end
%     end
 end
timeHigh = ((tofHigh-44)./2.372530695);
timeLow  = ((tofLow-44)./2.372530695);


% calculate Ranges using maximum of center

figure(1);
hold on;
colormap jet;
% xSub = [dvs, dvs(length(dvs))];
% ySub = [tofCh, tofCh(length(tofCh))];
[x,y] = meshgrid(tofCh,dvs);
pcolor(x,y,dataPlot');
colorbar;
shading flat;
axis([0 1023 0 31]);
set(gca,'Fontsize',22,'Fontweight','bold','linewidth',2);
plot(tofLow,dvs,'k','linewidth',2);
plot(tofHigh,dvs,'k','linewidth',2);
plot(TCHion,dvs,'k--');
xlabel('TOF Digital Channel');
ylabel('DVS Step');
title('TOF Tracks for Ne+');
axis([65 1024 0 31]);
hold off;

figure(2);
hold on;
colormap jet;
xSub = [eoq, eoq(length(eoq))];
ySub = [tofs, tofs(length(tofs))];
[x,y] = meshgrid(tofs,eoq);
pcolor(x,y,dataPlot');
colorbar;
shading flat;
set(gca,'Fontsize',22,'Fontweight','bold','linewidth',2);
% plot(tofLow,dvs,'c','linewidth',2);
% plot(tofHigh,dvs,'c','linewidth',2);
plot(time,eoq,'k','linestyle','-.');
plot(timeLow,eoq,'k','linewidth',2);
plot(timeHigh,eoq,'k','linewidth',2);
xlabel('TOF [ns]');
ylabel('Energy/Charge');
title('TOF Tracks for Ne+');
axis([0 250 6.19 223.12]); 
hold off;

%% Write the TOF track file

fid = fopen('tof_ranges_Ne+.dat','w');
for i = 1:length(dvs)
    fprintf(fid,'%6.1f    %6.1f\n',tofLow(i), tofHigh(i));
end
status = fclose(fid);
