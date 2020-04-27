% TRIManalysis.m
% 15 April 2011, Jason Gilbert

% This takes output data from TRIM and plots a histogram of the final 
% energy or the energy loss, with statistics, that an ion experiences 
% through a carbon foil.

clear all;
close all;

% filename = ['26.9_keV_Mg_67-C.txt';'35.9_keV_Mg_67-C.txt';'55.9_keV_Mg_67-C.txt';'95.9_keV_Mg_67-C.txt';];
% filename = ['  5_keV_He_111-C.txt';' 10_keV_He_111-C.txt';' 25_keV_He_111-C.txt';' 35_keV_He_111-C.txt';' 55_keV_He_111-C.txt';' 85_keV_He_111-C.txt';'110_keV_He_111-C.txt';'150_keV_He_111-C.txt';'175_keV_He_111-C.txt';'200_keV_He_111-C.txt'];
% filename = [' 25_keV_N_111-C.txt';' 35_keV_N_111-C.txt';' 55_keV_N_111-C.txt';' 70_keV_N_111-C.txt';' 85_keV_N_111-C.txt';'110_keV_N_111-C.txt';'130_keV_N_111-C.txt';'150_keV_N_111-C.txt';'175_keV_N_111-C.txt';'200_keV_N_111-C.txt';'250_keV_N_111-C.txt';'275_keV_N_111-C.txt'];
% filename = [' 25_keV_Fe_111-C.txt';' 50_keV_Fe_111-C.txt';' 75_keV_Fe_111-C.txt';'100_keV_Fe_111-C.txt';'125_keV_Fe_111-C.txt';'150_keV_Fe_111-C.txt';'175_keV_Fe_111-C.txt';'200_keV_Fe_111-C.txt'];
% filename = [' 25_keV_C_111-C.txt';' 50_keV_C_111-C.txt';' 75_keV_C_111-C.txt';'100_keV_C_111-C.txt';'125_keV_C_111-C.txt';'150_keV_C_111-C.txt';'175_keV_C_111-C.txt';'200_keV_C_111-C.txt'];
% filename = [' 25_keV_O_111-C.txt';' 35_keV_O_111-C.txt';' 55_keV_O_111-C.txt';' 85_keV_O_111-C.txt';'110_keV_O_111-C.txt';'150_keV_O_111-C.txt';'200_keV_O_111-C.txt';'250_keV_O_111-C.txt';'275_keV_O_111-C.txt'];
% filename = [' 25_keV_Fe_111-C.txt';' 50_keV_Fe_111-C.txt';' 75_keV_Fe_111-C.txt';'100_keV_Fe_111-C.txt';'125_keV_Fe_111-C.txt';'150_keV_Fe_111-C.txt';'175_keV_Fe_111-C.txt';'200_keV_Fe_111-C.txt'];
%filename = ['  1_keV_H_89-C.txt';'  5_keV_H_89-C.txt';' 10_keV_H_89-C.txt';' 25_keV_H_89-C.txt';' 50_keV_H_89-C.txt';' 75_keV_H_89-C.txt';'100_keV_H_89-C.txt';'125_keV_H_89-C.txt';'150_keV_H_89-C.txt';'175_keV_H_89-C.txt';'200_keV_H_89-C.txt';'225_keV_H_89-C.txt'];
% filename = ['  O6+_37-14Kev_88-77Ang.txt'; '  O6+_41-70Kev_88-77Ang.txt';'  O6+_46-81Kev_88-77Ang.txt'; '  O6+_52-55Kev_88-77Ang.txt'; '  O6+_58-99Kev_88-77Ang.txt'; '  O6+_66-22Kev_88-77Ang.txt'; ...
%             '  O6+_74-34Kev_88-77Ang.txt'; '  O6+_83-45Kev_88-77Ang.txt';'  O6+_93-68Kev_88-77Ang.txt'; ' O6+_105-16Kev_88-77Ang.txt'; ' O6+_118-06Kev_88-77Ang.txt'; ' O6+_132-53Kev_88-77Ang.txt'; ...
%             ' O6+_148-77Kev_88-77Ang.txt'; ' O6+_167-01Kev_88-77Ang.txt';' O6+_187-48Kev_88-77Ang.txt'; ' O6+_210-47Kev_88-77Ang.txt'; ' O6+_236-27Kev_88-77Ang.txt'; ' O6+_265-23Kev_88-77Ang.txt'; ...
%             ' O6+_297-74Kev_88-77Ang.txt'; ' O6+_334-24Kev_88-77Ang.txt';' O6+_375-22Kev_88-77Ang.txt'; ' O6+_421-21Kev_88-77Ang.txt'; ' O6+_472-85Kev_88-77Ang.txt'; ' O6+_530-81Kev_88-77Ang.txt'; ...
%             ' O6+_595-88Kev_88-77Ang.txt'; ' O6+_668-93Kev_88-77Ang.txt';' O6+_750-93Kev_88-77Ang.txt'; ' O6+_842-98Kev_88-77Ang.txt'; ' O6+_946-32Kev_88-77Ang.txt'; 'O6+_1062-33Kev_88-77Ang.txt'; ...
%             'O6+_1192-55Kev_88-77Ang.txt'; 'O6+_1338-74Kev_88-77Ang.txt';];
filename = ['  C+_6-19Kev_88-77Ang.txt'; '  C+_6-95Kev_88-77Ang.txt';'  C+_7-80Kev_88-77Ang.txt'; '  C+_8-76Kev_88-77Ang.txt'; '  C+_9-83Kev_88-77Ang.txt'; ' C+_11-04Kev_88-77Ang.txt'; ...
            ' C+_12-39Kev_88-77Ang.txt'; ' C+_13-91Kev_88-77Ang.txt';' C+_15-61Kev_88-77Ang.txt'; ' C+_17-53Kev_88-77Ang.txt'; ' C+_19-68Kev_88-77Ang.txt'; ' C+_22-09Kev_88-77Ang.txt'; ...
            ' C+_24-80Kev_88-77Ang.txt'; ' C+_27-84Kev_88-77Ang.txt';' C+_31-25Kev_88-77Ang.txt'; ' C+_35-08Kev_88-77Ang.txt'; ' C+_39-38Kev_88-77Ang.txt'; ' C+_44-21Kev_88-77Ang.txt'; ...
            ' C+_49-62Kev_88-77Ang.txt'; ' C+_55-71Kev_88-77Ang.txt';' C+_62-54Kev_88-77Ang.txt'; ' C+_70-20Kev_88-77Ang.txt'; ' C+_78-81Kev_88-77Ang.txt'; ' C+_88-47Kev_88-77Ang.txt'; ...
            ' C+_99-31Kev_88-77Ang.txt'; 'C+_111-49Kev_88-77Ang.txt';'C+_125-16Kev_88-77Ang.txt'; 'C+_140-50Kev_88-77Ang.txt'; 'C+_157-72Kev_88-77Ang.txt'; 'C+_177-05Kev_88-77Ang.txt'; ...
            'C+_198-76Kev_88-77Ang.txt';'C+_223-12Kev_88-77Ang.txt'];

        
Eplot = 1; % 0 = Plot the final energy, 1 = Plot the energy loss

E0 = [6.19, 6.94, 7.80, 8.75, 9.83, 11.03, 12.39, 13.91, 15.61, 17.53, 19.68, 22.09, ...
      24.80, 27.84,31.25, 35.08, 39.38, 44.21, 49.62, 55.71, 62.54, 70.20, 78.81, 88.47, ...
      99.31, 111.49, 125.16, 140.50, 157.72, 177.05, 198.76, 223.12];
E0 = E0.*1;
E0 = E0'.*1000;
numParts = 99999;

Eloss = zeros(numParts,size(filename,1));
%E0 = zeros(size(filename,1),1);
HighEnergy = zeros(size(filename,1),1);
LowEnergy = zeros(size(filename,1),1);

for sindex = 1:size(filename,1)
    %file = ['C:\Users\jagi.UMROOT\Documents\TRIM_Data\',strtrim(filename(sindex,:))];
    file = [pwd,'/20110303_SRIMCalc_TOF_Revamp/C+/',strtrim(filename(sindex,:))];
    fid = fopen(file, 'r');
    clear headinfo;
    clear data;
    [headinfo] = textscan(fid,'%80c',1,'headerLines',9);
    [data] = textscan(fid,'%1s %5d %3d %13f %14f %11f %11f %11f %10f %10f','headerLines',2);
    fclose(fid);

    Energy = zeros(numParts,1);
    paren = find(headinfo{1}=='(');
    parenend = find(headinfo{1}==')');
    Atom = strtrim(headinfo{1}(paren(1)-3:paren(1)-1));
    %E0(sindex,1) = str2double(headinfo{1}(paren(1)+1:parenend(1)-4))*1000; % The initial energy (eV)
    foil = str2double(headinfo{1}(paren(2)+1:parenend(2)-3))/10; % The foil thickness (nm)
    Energy(1:size(data{4})) = data{4}; % The final energy (eV)
    Eloss(:,sindex) = E0(sindex,1)-Energy; % The energy loss (eV)
    
    if Eplot==0 % Plot the post-foil final energy 
        nonzeroE = (Energy(Energy>0))./1000;
    else % Plot the energy lost in the carbon foil
        nonzeroE = (Eloss(Energy>0,sindex))./1000;
    end   
    
    figure
    hold on
    [n, xouts] = hist(nonzeroE,500);
    hist(nonzeroE, 500);
    cdfPer = zeros(500,1);
    for i = 1:500
        cdfPer(i) = sum(n(1:i))/sum(n);
    end
    check = 0;
    i = 1;
    while (check == 0)
        if (cdfPer(i) > 0.25) 
            check = 1;
            LowEnergy(sindex) = xouts(i);
        end
        i=i+1;
    end
    
    check = 0;
    i = 1;
    while (check == 0)
        if (cdfPer(i) > 0.75) 
            check = 1;
            HighEnergy(sindex) = xouts(i);
        end
        i=i+1;
    end       
        
    if Eplot==0
        title(['Post-foil energy of ',Atom(1,:),' for E_0 = ',num2str(E0(sindex,1)/1000),' keV'],'Fontsize',14);
        xlabel('Residual energy after foil transmission (keV)','Fontsize',14);
%         text(.10*max(y), .60*max(m), ['peak Energy = ',num2str(gaussmaxenergy(sindex,1),5),' keV'])
%         text(.10*max(y), .50*max(m), ['Standard dev = ',num2str(gaussstddev(sindex,1),5),' keV'])
%         text(.10*max(y), .70*max(m), ['foil thickness = ',num2str(foil*10),' Ang'])
    else
        title(['Energy loss of ',Atom(1,:),' for E_0 = ',num2str(E0(sindex,1)/1000),' keV'],'Fontsize',14);
        xlabel('Energy lost during foil transmission (keV)','Fontsize',14);
        plot([LowEnergy(sindex), LowEnergy(sindex)],[0, max(n)*1.20], '-r');
        plot([HighEnergy(sindex), HighEnergy(sindex)],[0, max(n)*1.20], '-r');
%         text(15, .60*max(m), ['peak Energy loss = ',num2str(gaussmaxenergy(sindex,1),5),' keV'])
%         text(15, .50*max(m), ['Standard dev = ',num2str(gaussstddev(sindex,1),5),' keV'])
%         text(15, .70*max(m), ['foil thickness = ',num2str(foil*10),' Ang'])
        xlim([0 20])
        ylim([0 max(n)*1.15]);
    end
    ylabel('Counts','Fontsize',14);
    hold off
    legend off
    box on

end

E0keV = E0./1000;

%--------------------Two-term exponential fit------------------------------
f5 = fittype('Exp2'); % Settings for an exponential fit in Matlab
gfit2 = fit(E0keV,LowEnergy,f5);
p1=gfit2.a; % Coefficients to the fit of ELoss vs. Einitial
p2=gfit2.b;
p3=gfit2.c;
p4=gfit2.d;

disp('Coefficients for Eloss_Low = p1*exp(p2*Einit) + p3*exp(p4*Einit)')
disp(['p1 = ',num2str(p1)]);
disp(['p2 = ',num2str(p2)]);
disp(['p3 = ',num2str(p3)]);
disp(['p4 = ',num2str(p4)]);
equationLow = (['E_{loss} = ',num2str(p1),'*exp(',num2str(p2),'*E_0) + ',num2str(p3),'*exp(',num2str(p4),'*E_0)']);

%--------------------Two-term exponential fit------------------------------
f5 = fittype('Exp2'); % Settings for an exponential fit in Matlab
gfit3 = fit(E0keV,HighEnergy,f5);
p1=gfit3.a; % Coefficients to the fit of ELoss vs. Einitial
p2=gfit3.b;
p3=gfit3.c;
p4=gfit3.d;

disp('Coefficients for Eloss_High = p1*exp(p2*Einit) + p3*exp(p4*Einit)')
disp(['p1 = ',num2str(p1)]);
disp(['p2 = ',num2str(p2)]);
disp(['p3 = ',num2str(p3)]);
disp(['p4 = ',num2str(p4)]);
equationHigh = (['E_{loss} = ',num2str(p1),'*exp(',num2str(p2),'*E_0) + ',num2str(p3),'*exp(',num2str(p4),'*E_0)']);


%--------------------------Eloss vs E0--------------------------------------
figure
hold on
% for i=1:length(E0keV)
%     errorbar(E0keV(i,1),gaussmaxenergy(i,1),gaussstddev(i,1),gaussstddev(i,1),'-ok','MarkerFaceColor','k','MarkerSize',6)
% end
plot(E0keV,LowEnergy,'o','LineWidth',2,...
            'MarkerEdgeColor','k',...
            'MarkerFaceColor','k',...
            'MarkerSize',6)
plot(gfit2, 'r')
plot(E0keV,HighEnergy,'o','LineWidth',2,...
            'MarkerEdgeColor','k',...
            'MarkerFaceColor','k',...
            'MarkerSize',6)
plot(gfit3, 'b')        
hold off
title(['Energy loss of ',Atom(1,:),' through ',num2str(foil*10),'-Ang Carbon foil'],'FontSize',14,'FontWeight','Bold')
xlabel('Initial Energy (keV)','FontSize',14,'FontWeight','Bold')
ylabel('Energy loss (keV)','FontSize',14,'FontWeight','Bold')
text(25,0.25,equationLow)
text(25,0.50,equationHigh)
