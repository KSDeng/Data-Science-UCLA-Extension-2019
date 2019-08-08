close all;clear();clc();
training = importdata('training_set.csv');
training_data = training.data;

predictors = [3,10,11,12,15];   % POP, UR, GR, SAV, GDPC
input_vars = training_data(:,predictors)';    % input
mar = training_data(:,13)';               % target

% minmax
[input_vars_n, input_str] = mapminmax(input_vars);
[mar_n, output_str] = mapminmax(mar);


net = newff(input_vars_n, mar_n, [5,15,1],{'purelin','logsig','purelin'});

net.trainParam.lr = 0.01;
%net.trainParam.epochs = 5000;
net = train(net,input_vars_n,mar_n);
sim_out= sim(net, input_vars_n);

sim_res = mapminmax('reverse',sim_out,output_str);

x = 1:length(mar);
plot(x,mar,'b','LineWidth',2);
hold on; plot(x, sim_res,'r','LineWidth',2);
legend('Actual','Predict');xlabel('Index');ylabel('Number of Marriage Couples(10,000)');
%title('NN Performance in Training Set');

% Evaluate on testing set.
testing = importdata('testing_set.csv');
testing_data = testing.data;
t_input = testing_data(:,predictors)';
t_mar = testing_data(:,13)';
[t_input_n, t_input_str] = mapminmax(t_input);
[t_mar_n, t_output_str] = mapminmax(t_mar);
t_sim_out = sim(net, t_input_n);
t_sim_res = mapminmax('reverse', t_sim_out, t_output_str);

% Visulization
x = 1:length(t_mar);
figure(2);
plot(x,t_mar,'b','LineWidth',2);
hold on; plot(x, t_sim_res,'r','LineWidth',2);
legend('Actual','Predict');xlabel('Index');ylabel('Number of Marriage Couples(10,000)');
%title('NN Performance in Testing Set');
sum((t_mar - t_sim_res).^2)/length(t_mar)


% Predict population
population = training_data(:,3);

