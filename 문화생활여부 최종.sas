/*��ȭ��Ȱ ����*/
PROC import DBMS=csv
Datafile='C:\Users\Yuna park\Desktop\�м��� ����\Tculture.csv'
out=culture
Replace;
Run;
/*���߰�����*/
PROC CORR DATA=CULTURE NOSIMPLE ALPHA;
VAR X1 X2 X4-X16;
RUN;
/*���� ���ù�*/
PROC LOGISTIC DATA=CULTURE DESC;
CLASS X1 X2 X4-X16;
MODEL Y=X1 X2 X4-X16/SELECTION=BACKWARD SLSTAY=0.1;
RUN;
/*������ƽ ȸ��*/
PROC Genmod Data=Culture Desc;
Class x9 x10;
Model y=x9 x10 /dist=bin link=logit LRCI Type3;
Run;
PROC LOGISTIC DATA=CULTURE DESC;
CLASS X9 X10 ;
MODEL Y=X9 X10 ;
RUN;
