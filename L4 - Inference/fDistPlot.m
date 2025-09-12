x = linspace(0,3.5,100);
y = fpdf(x,3,10000);

x2 = linspace(2.6049,3.5,100);
y2 = fpdf(x2,3,10000);

str = '.05 of Total Area';

close all
figure
hold on
plot(x,y)
area(x2,y2)
ylabel('PDF of $F_{3,\infty}$','Interpreter','latex')
xlabel('$F$','Interpreter','latex')

text(x2(1)+.15,y2(1)+.1,str,'Interpreter','latex')
annotation('arrow','X',[.8,.75],'Y',[.25,.18])

title('The F Distribution for 3 Restrictions')