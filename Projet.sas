/*Remarque sur le programme: il est TRES similaire au programme deroule en cours au tp5 ecrit par Vincent Gallman


/*Import de la base d'etude*/

LIBNAME IN "C:/Users/louis/Documents/M1/SASUniversityEdition/myfolders/sasuser.v94/Projet";

/*C'est un copie-colle du code execute par l'assistant d'import de donnees de SasUniversity*/
PROC SQL;
CREATE TABLE WORK.query AS
SELECT ID_CLIENT , retour , sexe , tel , email , age , revenu , nb_tot , mt_tot , mt_1er , mt_der , media_1er , anc_1er , anc_der , mt_encours , mt_der_paye , nb_encours , media_der , famille , resid , anc_remb FROM WORK.Base_score;
RUN;
QUIT;

PROC DATASETS NOLIST NODETAILS;
CONTENTS DATA=WORK.query OUT=WORK.details;
RUN;

PROC PRINT DATA=WORK.details;
RUN;
/*Je ne comprends pas ce code*/
/*nb: je dois ensuite coller manuellement le fichier dans "WORK"

/*ANALYSE DES DONNEES*/

/*1.Analyse univariee des donnees Qualtitatives*/

/*Variables Qualitatives:
age anc_remb email famille media_1er media_der nb_encours nb_tot resid retour sexe tel*/

PROC FREQ DATA = BASE_SCORE;
	TABLES anc_remb email famille
	media_1er media_der nb_encours nb_tot resid retour sexe tel/ MISSING;
RUN;

/*Resultats:
-Horizon prevue pour le remboursement & nombre de credits en cours: regrouper en 6+ pour equilibrer les modalites et eviter les dernieres modalites avec 1 seul individu
-Adresse E-mail & Tel: je m'interroge sur la pertinence de ces variables.
-Situation familiale: ras
-Medium 1er/dernier credit: ras
On remarque que la proportion de direct et mailing a augmente du 1er au dernier credit 
-Nombre total de credits: A regrouper en classes pour equilibre les modalites entre elles les dernieres n'etant quasiment pas representes.
-Logement,sexe ras il faudarait expliquer a sas que ?=missing
-retour ras

/*2.Analyse univariee des variables Quantitatives*/

/*Rq: on garde Anc_1er et Anc_der en quantitatif plutot que de regrouper en annees et les traiter en qualitatif car 
c'est assez rare d'avoir cette information en jours et peut ajouter une vraie information cad: il y a 317j 100 personnes d'un coup ont souscrit a un credit
alors pourquoi 317j exactement? Soldes???*/

/*Les variables quantitatives:
-mt_tot mt_1er mt_der mt_encours mt_der_paye anc_1er anc_der revenu*/

PROC MEANS DATA = BASE_SCORE N NMISS MIN Q1 MEDIAN MEAN Q3 MAX;
	VAR age mt_tot mt_1er mt_der mt_encours mt_der_paye anc_1er anc_der revenu;
RUN;

/*Quelques remarques:
Il manque quelques donnees pour anc_1er et anc_der
Le montant du dernier credit est en general plus eleve que celui du 1er...
Pour mt_total mt_1er mt_der mt_encours et revenus il y a des max tres eloignes des indicateurs de dispersion:
peu d'individus ont beaucoup de revenus et de credits (en montant) par rapport aux autres
Pout mt_1er on voit quelques crÃ©dits negatifs et pas pour mt_der: une offre de la banque pour son 1er credit???
Il y a peut etre une donnee a enlever car mt_total est certainement explicable avec mt_1er mt_der et mt_encours...
Pour le revenu il y a (au moins) un revenu tres eleve mais une moyenne assez proche de la mediane; c'est un individu particulierement extreme.
La moitie des gens ont deja pris un credit pendant l'annee*/

/*3. Analyse bivariee des variables qualitatives*/

/*Pour rappel la variable d'interet est retour*/

/*Profil ligne*/
PROC FREQ DATA = BASE_SCORE;
	TABLES RETOUR / NOCUM;
RUN;

/* Profils colonne */

PROC FREQ DATA = BASE_SCORE;
	TABLES RETOUR * (anc_remb email famille media_1er media_der nb_encours nb_tot resid sexe tel) / MISSING NOROW NOCUM NOPERCENT;
RUN;

/*does not work (no error but infinite runtime)*/

/* Graphiques : Diagrammes en barres empilees */
ODS GRAPHICS ON;
PROC FREQ DATA = BASE_SCORE ORDER = FREQ;
	TABLES RETOUR * (anc_remb email famille media_1er media_der nb_encours nb_tot resid sexe tel) / PLOTS = FREQPLOT(TWOWAY = STACKED SCALE = GROUPPCT); 
RUN;

/*Quelques Remarques:
-duree de remboursement du dernier credit: a partir de 9 les resultats sont binaires (oui a 100% ou non a 100%)
Peu d'individus sont dans ces modalites: il faut les regrouper.
-e-mail: je ne sais pas pourquoi mais il y a graphiquement une nettes difference entre ceux ayant
renseignÃ©s leur email et ceux ne l'ayant pas. Ces premiers ont nettement plus souvent un credit
-tel: encore plus surprenant; le fait de renseigner ou pas son telephone n'a en revanche pas
l'air de jouer le moindre impact
-La famille: il y a clairement 2 regroupements a faire: divorce/celib et marie/union l; ces premeirs ayant
l'air d'avoir plus souvent acces a des credits.
-medium 1er credit: on peut regrouper retail/mailing et on observera une nette difference avec direct
-medium dernier credit: on peut regrouper mailig/direct pour observer une nette difference avec retail
Mailing a plus de succes au dernier credit qu'au premeier credit.
-Credit en cours: regrouper les dernieres cellules regroupant trop peu d'individus pour dire quoique ce soit
Quoiqu'on a reaccorde des credits a certains ayant deja 9 credits en cours...
-nb total: de meme pour les cas extremes (27,31,34 et 38): c'est binaire (1 ou 2 individus extremes); regrouper.
-resid: ? a fusionner avec loc et on observera une difference
-sexe: variable pertinente: on accorde plus de credits aux femmes!*/

/* Test statistique : X2 et V-Cramer */

ODS OUTPUT CHISQ = CHISQ; 
PROC FREQ DATA = BASE_SCORE ;
  TABLES (anc_remb email famille media_1er media_der nb_encours nb_tot resid sexe tel  ) * RETOUR / CHISQ;
RUN;
ODS SELECT ALL;

/* sans surprises toutes les variables sont tres significatives (voire extremement significatives- aux warnings
peu d'individus par modalites pres) a part le tel (juste significative) (pourquoi le mail et pas le tel???)
*/

/*Pas de donnees a enlever etant toutes significatives*/

/* Variables non significatives */

DATA PAS_SIGNIF (KEEP = VARIABLE);  
  SET CHISQ; 
  
	WHERE STATISTIC = 'Khi-2' AND PROB > .05;
	    
	LENGTH VARIABLE $15.;
	
	VARIABLE     = SCAN (TABLE,2); 

RUN;

/* 4.Variables quantitatives */
/*age mt_tot mt_1er mt_der mt_encours mt_der_paye anc_1er anc_der revenu*/

TITLE "Retour vs Age";
PROC SGPLOT DATA = BASE_SCORE;
	VBOX AGE / CATEGORY = RETOUR;
	YAXIS GRID;
RUN;

TITLE "Retour vs Montant total";
PROC SGPLOT DATA = BASE_SCORE;
	VBOX mt_tot / CATEGORY = RETOUR;
	YAXIS GRID;
RUN;

TITLE "Retour vs montant 1er credit";
PROC SGPLOT DATA = BASE_SCORE;
	VBOX mt_1er / CATEGORY = RETOUR;
	YAXIS GRID;
RUN;

TITLE "Retour vs montant dernier credit";
PROC SGPLOT DATA = BASE_SCORE;
	VBOX mt_der / CATEGORY = RETOUR;
	YAXIS GRID;
RUN;

TITLE "Retour vs Montant des credits en cours";
PROC SGPLOT DATA = BASE_SCORE;
	VBOX mt_encours / CATEGORY = RETOUR;
	YAXIS GRID;
RUN;

TITLE "Retour vs montant de la derniere echeance";
PROC SGPLOT DATA = BASE_SCORE;
	VBOX mt_der_paye / CATEGORY = RETOUR;
	YAXIS GRID;
RUN;

TITLE "Retour vs Anciennete du 1er credit";
PROC SGPLOT DATA = BASE_SCORE;
	VBOX anc_1er / CATEGORY = RETOUR;
	YAXIS GRID;
RUN;

TITLE "Retour vs Anciennete du dernier credit";
PROC SGPLOT DATA = BASE_SCORE;
	VBOX anc_der / CATEGORY = RETOUR;
	YAXIS GRID;
RUN;

TITLE "Retour vs Revenu";
PROC SGPLOT DATA = BASE_SCORE;
	VBOX revenu / CATEGORY = RETOUR;
	YAXIS GRID;
RUN;

/*Commentaires:
-Retour vs age: difference peu marquee mais semble existantante
-Retour vs Montant total: la difference semble marquee *
-Retour vs montant 1er credit: peu de difference *
-Retour vs montant dernier credit: difference peu marquant (un peu sur la mediane)
-Retour vs montant en cours: a l'air pertinente difference assez marquee
-Retour vs montant derniere echeance: **
-Retour vs anciennete 1er credit: aucune pertinenece graphique
-Retour vs anciennete dernier credit: petite difference
On remarque un saut assez imprevu sur la duree en jour du dernier credit
-Retour vs revenu:**
*: les individus extremes mentionnes precedemment genent a l'analyse graphique
**:* en pire ils rendent impossible l'analyse graphique
Une selection des individus peut s'averer utile car il serait peu surprenant que les individus a haut
revenu soient aussi ceux qui ont emprunte le plus, depuis le plus longtemps etc.
*/

/*Tests statistiques*/

/* Si la distribution est normale : Test de student        */
/* Si la distribution est pas normale : Test de Kruskal Wallis */

ODS GRAPHICS ON;
PROC UNIVARIATE DATA = BASE_SCORE PLOT NORMALTEST;
	VAR age mt_tot mt_1er mt_der mt_encours mt_der_paye anc_1er anc_der revenu;
RUN;
/*Ne tourne pas...*/

/* Test d'egalite des moyennes : Student */

/*PROC TTEST DATA = BASE_SCORE;
   CLASS RETOUR;
   VAR ;
RUN;*/

/*Aucune variable n'est normalement distribuee d'apres le qq-plot

/* Test de Kruskal Wallis */

ODS OUTPUT KRUSKALWALLISTEST = KRUSKAL; 
PROC NPAR1WAY WILCOXON DATA = BASE_SCORE; 
  CLASS RETOUR;
  VAR age mt_tot mt_1er mt_der mt_encours mt_der_paye anc_1er anc_der revenu;
RUN;
ODS SELECT ALL;

/*Toutes les donnees sont pertinentes sauf anc_1er comme vu plus haut*/

/*5.Recodage des donnees

/*Discretisation donnees quantitatives*/
/*Obligatoire pour traiter le cas des hauts revenus*/
/*Le nombre de classes a Ã©tÃ© trouvÃ© en experimentant (on cherchait a creer dans les analyses bivariees
des differences significatives entre les classes*/

/*Age*/
PROC RANK DATA = BASE_SCORE OUT = BASE_SCORE GROUPS = 6;
	RANKS AGE_TR;
	VAR AGE;
RUN;

PROC MEANS DATA = BASE_SCORE N MIN MAX MAXDEC = 0;
	CLASS AGE_TR;
	VAR AGE;
RUN;
PROC FREQ DATA = BASE_SCORE;
	TABLES retour * AGE_TR / MISSING NOROW NOCUM NOPERCENT;
RUN;
/*si on veut regrouper les ages*/

/* REVENU */
PROC RANK DATA = BASE_SCORE OUT = BASE_SCORE GROUPS = 7;
	RANKS REVENU_TR;
	VAR REVENU;
RUN;

PROC MEANS DATA = BASE_SCORE N MIN MAX MAXDEC = 0;
	CLASS REVENU_TR;
	VAR REVENU;
RUN;
PROC FREQ DATA = BASE_SCORE;
	TABLES RETOUR * REVENU_TR / MISSING NOROW NOCUM NOPERCENT;
RUN;

/*Montant total*/
PROC RANK DATA = BASE_SCORE OUT = BASE_SCORE GROUPS = 7;
	RANKS mt_tot_TR;
	VAR mt_tot;
RUN;

PROC MEANS DATA = BASE_SCORE N MIN MAX MAXDEC = 0;
	CLASS mt_tot_TR;
	VAR mt_tot;
RUN;
PROC FREQ DATA = BASE_SCORE;
	TABLES RETOUR * mt_tot_TR / MISSING NOROW NOCUM NOPERCENT;
RUN;

/*Montant 1er credit*/
PROC RANK DATA = BASE_SCORE OUT = BASE_SCORE GROUPS = 4;
	RANKS mt_1er_TR;
	VAR mt_1er;
RUN;

PROC MEANS DATA = BASE_SCORE N MIN MAX MAXDEC = 0;
	CLASS mt_1er_TR;
	VAR mt_1er;
RUN;
PROC FREQ DATA = BASE_SCORE;
	TABLES RETOUR * mt_1er_TR / MISSING NOROW NOCUM NOPERCENT;
RUN;

/*Montant dernier credit*/
PROC RANK DATA = BASE_SCORE OUT = BASE_SCORE GROUPS = 7;
	RANKS mt_der_TR;
	VAR mt_der;
RUN;

PROC MEANS DATA = BASE_SCORE N MIN MAX MAXDEC = 0;
	CLASS mt_der_TR;
	VAR mt_der;
RUN;
PROC FREQ DATA = BASE_SCORE;
	TABLES RETOUR * mt_der_TR / MISSING NOROW NOCUM NOPERCENT;
RUN;

/*Anciennete du 1er credit*/
PROC RANK DATA = BASE_SCORE OUT = BASE_SCORE GROUPS = 4;
	RANKS anc_1er_TR;
	VAR anc_1er;
RUN;

PROC MEANS DATA = BASE_SCORE N MIN MAX MAXDEC = 0;
	CLASS anc_1er_TR;
	VAR anc_1er;
RUN;
PROC FREQ DATA = BASE_SCORE;
	TABLES RETOUR * anc_1er_TR / MISSING NOROW NOCUM NOPERCENT;
RUN;

/*Anciennete du dernier credit*/
PROC RANK DATA = BASE_SCORE OUT = BASE_SCORE GROUPS = 4;
	RANKS anc_der_TR;
	VAR anc_der;
RUN;

PROC MEANS DATA = BASE_SCORE N MIN MAX MAXDEC = 0;
	CLASS anc_der_TR;
	VAR anc_der;
RUN;
PROC FREQ DATA = BASE_SCORE;
	TABLES RETOUR * anc_der_TR / MISSING NOROW NOCUM NOPERCENT;
RUN;

/*Montant des credits en cours*/
PROC RANK DATA = BASE_SCORE OUT = BASE_SCORE GROUPS = 7;
	RANKS mt_encours_TR;
	VAR mt_encours;
RUN;

PROC MEANS DATA = BASE_SCORE N MIN MAX MAXDEC = 0;
	CLASS mt_encours_TR;
	VAR mt_encours;
RUN;
PROC FREQ DATA = BASE_SCORE;
	TABLES RETOUR * mt_encours_TR / MISSING NOROW NOCUM NOPERCENT;
RUN;

/*Montant de la derniere echeance*/
PROC RANK DATA = BASE_SCORE OUT = BASE_SCORE GROUPS = 5;
	RANKS mt_der_paye_TR;
	VAR mt_der_paye;
RUN;

PROC MEANS DATA = BASE_SCORE N MIN MAX MAXDEC = 0;
	CLASS mt_der_paye_TR;
	VAR mt_der_paye;
RUN;
PROC FREQ DATA = BASE_SCORE;
	TABLES RETOUR * mt_der_paye_TR / MISSING NOROW NOCUM NOPERCENT;
RUN;

/*Regroupements*/
DATA BASE_SCORE;
	SET BASE_SCORE;

/*Qualitatives*/
/*email bien code*/
/*tel a jeter*/

top_f = ( SEXE = 'F' );
top_h = ( SEXE = 'M' );
	
media_1er_mailing_retail = ( media_1er IN ('mailing','retail') );
media_1er_direct         = ( media_1er = 'direct' );

media_der_mailing_direct=(media_der IN ('mailing','direct'));
media_der_retail=(media_der='retail');

famille_divorce_celib=(famille IN ('CÃ©lib','DivorcÃ©'));
famille_unionl_marie=(famille IN ('Union l','MariÃ©','?'));

resid_loc_uknown=(resid IN ('?','Loc'));
resid_propr=(resid IN('Propr'));

anc_remb_1_2=(anc_remb<=2);
anc_remb_3_5=(3<=anc_remb<=5);
anc_remb_6ormore=(anc_remb>=6);

nb_encours_1_2=(nb_encours<=2);
nb_encours_3_5=(3<=nb_encours<=5);
nb_encours_6ormore=(nb_encours>=6);

nb_tot_1_2=(nb_tot<=2);
nb_tot_3_5=(3<=mbtot<=5);
nb_tot_6_9=(6<=nb_tot<=9);
nb_tot_10ormore=(nb_tot>=10);

/*Discretisation variables quantitatives*/

/*
age_18_30=(18<=age<=30)
age_31_37=(31<=age<=37)
age_38_42=(38<=age<=42)
age_43_47=(43<=age<=47)
age_48_52=(48<=age<=52)
age_53_59=(53<=age<=59)

revenu_0_1200=(0<=revenu<=1200)
revenu_1201_1466=(1201<=revenu<=1466)
revenu_1467_1646=(1467<=revenu<=1646)
revenu_1647_1867=(1647<=revenu<=1867)
revenu_1868_2052=(1868<=revenu<=2052)
revenu_2053_2400=(2053<=revenu<=2400)
revenu_2401ormore=(revenu>=2401)

mt_tot_210_1200=(210<=mt_tot<=1200)
mt_tot_1201_2000=(1201<=mt_tot<=2000)
mt_tot_2001_3200=(2001<=mt_tot<=3200)
mt_tot_3201_4800=(3201<=mt_tot<=4800)
mt_tot_4801_6800=(4801<=mt_tot<=6800)
mt_tot_6801_10666=(6801<=mt_tot<=10666)
mt_tot_10667ormore=(mt_tot>=10667)

mt_1er_48_667=(-48<=mt_1er<=667)
mt_1er_668_1180=(668<=mt_1er<=1180)
mt_1er_1181_2000=(1181<=mt_1er<=2000)
mt_1er_2001ormore=(mt_1er>=2001)

mt_der_766=(173<=mt_der<=766)
mt_der_767_1067=(767<=mt_der<=1067)
mt_der_1068_1333=(1068<=mt_der<=1333)
mt_der_1334_1667=(1334<=mt_der<=1667)
mt_der_1668_2667=(1668<=mt_der<=2667)
mt_der_2668_4000=(2668<=mt_der<=4000)
mt_der_4000ormore=(mt_der>=4000)

anc_1er_3_485=(3<=anc_1er<=485)
anc_1er_486_1323=(486<=anc_1er<=1323)
anc_1er_1324_3286=(1324<=anc_1er<=3286)
anc_1er_3287ormore=(anc_1er>=3287)

anc_der_3_146=(3<=anc_der<=146)
anc_der_147_321=(147<=anc_der<=321)
anc_der_322_588=(322<=anc_der<=588)
anc_der_589_7304=(589<=anc_der<=7304)
*/

DROP tel sexe revenu resid nb_tot nb_encours anc_remb resid famille media_1er media_der age anc_1er anc_der anc_remb
ID_CLIENT mt_1er mt_der mt_der_paye mt_encours mt_tot ;
RUN;




/*Correlation*/


ODS LISTING CLOSE;
PROC CONTENTS DATA = BASE_SCORE NOPRINT OUT = CONT;
RUN;

PROC SQL;
	SELECT DISTINCT NAME INTO: LISTE_VAR SEPARATED BY ' '
	FROM CONT
	WHERE NAME NE 'ID';
QUIT;

ODS OUTPUT CHISQ = CHISQ (KEEP  = TABLE VALUE STATISTIC
						  WHERE = ( UPCASE(STATISTIC) LIKE '%CRAMER%')); 
PROC FREQ DATA = BASE_SCORE;
  TABLES (&LISTE_VAR) * (&LISTE_VAR) / CHISQ;
RUN;



DATA CHISQ_REPONSE (WHERE = ( VARIABLE1 < VARIABLE2))
	 CHISQ_AUTRE   (WHERE = ( VARIABLE1 < VARIABLE2));  
	SET CHISQ (DROP = STATISTIC); 
  
LENGTH VARIABLE1 VARIABLE2 $32.;

ABS_V_CRAMER = ABS(VALUE);
VARIABLE1 = SCAN (TABLE,2);
VARIABLE2 = SCAN (TABLE,3);

IF VARIABLE2 = 'retour' THEN OUTPUT CHISQ_REPONSE;
ELSE OUTPUT CHISQ_AUTRE;

RUN;

/*5. Modelisation*/


/* Separation Apprentissage / Test */


PROC SORT DATA = BASE_SCORE; 
	BY RETOUR; 
RUN;

PROC SURVEYSELECT DATA = BASE_SCORE
                  OUTALL		
				  SAMPRATE = 70	
                  OUT = BASE_SCORE (DROP = SELECTIONPROB SAMPLINGWEIGHT)
                  METHOD = SRS 
                  SEED = 2018; 	
  				  STRATA retour;				
RUN;

/* Separation base apprentissage et base de validation */
DATA APP  (DROP = SELECTED)
	 TEST (DROP = SELECTED);
	SET BASE_SCORE;

IF SELECTED = 1 THEN OUTPUT APP;
ELSE OUTPUT TEST;

RUN;

/* Verification que le taux de cible est le meme dans chaque echantillon */
PROC FREQ DATA = BASE_SCORE;
	TABLE RETOUR;
RUN;

PROC FREQ DATA = APP;
	TABLE RETOUR;
RUN;

PROC FREQ DATA = TEST;
	TABLE RETOUR;
RUN;
/*OK*/

/* Modelisation */

/* Choix du modele */
ODS GRAPHICS ON;
ODS OUTPUT FITSTATISTICS = CRITERES;
PROC LOGISTIC DATA = APP PLOTS = ROC;
	MODEL RETOUR (EVENT = LAST) =  anc_remb_1_2 anc_remb_3_5
	email famille_divorce_celib 
	media_1er_direct  media_der_mailing_direct 
	nb_encours_1_2 nb_encours_3_5 
	nb_tot_1_2 nb_tot_3_5 nb_tot_6_9
	resid_propr top_f										
												/ SELECTION = STEPWISE
												  SLE = .05 SLS = .05 
												  CTABLE PPROB=(0 TO 1 BY 0.01);
RUN;
ODS GRAPHICS OFF; 
/*ignored: anc_remb_6ormore famille_unionl_marie media_1er_mailing_retail	media_der_retail 
resid_loc_uknown	
nb_encours_6ormore	nb_tot_10ormore	top_h	resid_loc_uknown*/
/*Les graphiques ne sortent pas sur sas university*/

/*Pour la selection des variables on utilisera le critere AIC*/


 DATA AIC;
	SET CRITERES;
 WHERE CRITERION="AIC";
 RUN;

AXIS1 LABEL = ("Nombre de variables");
AXIS2 LABEL = ("AIC");
SYMBOL1 INTERPOL = JOIN;
 PROC GPLOT DATA = AIC;
 	PLOT INTERCEPTANDCOVARIATES * STEP / HAXIS = AXIS1 VAXIS = AXIS2 HREF = 4;
 RUN;	
/*Does not work on sas university*/		


/*Test sur la base de test*/

PROC LOGISTIC DATA = APP OUTMODEL = MODEL OUTEST = BETAS;
	MODEL RETOUR (EVENT = LAST) = anc_remb_1_2 anc_remb_3_5
	email famille_divorce_celib 
	media_1er_direct  media_der_mailing_direct 
	nb_encours_1_2 nb_encours_3_5 
	nb_tot_1_2 nb_tot_3_5 nb_tot_6_9
	resid_propr top_f;
	/* On applique le modele retenu sur la base d'apprentissage ... */
	SCORE DATA = APP OUT = APP_PRED;
	/* ... et sur la base de test */
	SCORE DATA = TEST OUT = TEST_PRED;
RUN;	
/*C'est pas mal il semble meme que les predictions se passent encore mieux sur la base de tests
/* Comparaison APP et TEST */
/*Je ne comprends absolument pas le code ci-dessous*/
%MACRO DIAGNOSTICS (TABLES, 
					VARSCORE, 
					VARTARGET, 
					OUT=, 
					EVENT=1);

	DATA WORK._SCORES (DROP = &VARTARGET);
		SET 
			%LET I=1;
			%LET TABLES = %CMPRES(&TABLES);
			%DO %WHILE(%SCAN(&TABLES,&I,%STR( )) NE);
				%SCAN(&TABLES,&I,%STR( )) (KEEP = &VARTARGET &VARSCORE IN = DANS&I)
				%LET I = %EVAL(&I+1);
			%END;
			%LET NBTABLES = %EVAL(&I-1);
		;
		LENGTH _DATA $ 35;
		_EVENT = (&VARTARGET = &EVENT);
		%DO I=1 %TO &NBTABLES;
			IF DANS&I THEN _DATA = "#&I %SCAN(&TABLES,&I,%STR( ))";
		%END;
	RUN;

	PROC SORT DATA = WORK._SCORES;
		BY DESCENDING _DATA
 		   _EVENT 
		   DESCENDING &VARSCORE;
	RUN;

	ODS EXCLUDE ALL;
	ODS OUTPUT WILCOXONSCORES = WORK.VALUES;
	PROC NPAR1WAY DATA = WORK._SCORES WILCOXON CORRECT = NO ANOVA;
		VAR &VARSCORE;
		CLASS _EVENT;
		BY DESCENDING _DATA;
	RUN;

	ODS OUTPUT SUMMARY = WORK.PROPORTION;
	PROC MEANS DATA = WORK._SCORES MEAN;
		VAR _EVENT;
		BY DESCENDING _DATA;
	RUN;

	PROC SQL;
		DROP TABLE WORK._SCORES;
	QUIT;

	DATA WORK.VALUES (KEEP = _DATA AUC AUL KI);
		MERGE WORK.VALUES
		 	  WORK.PROPORTION;

		BY DESCENDING _DATA;

		N0 = N; 
		R0 = SUMOFSCORES;
		N1 = LAG(N); 
		R1 = LAG(SUMOFSCORES);

		IF MOD(_N_,2) = 0 THEN DO;
			U1 = (N1*N0) + (N1*(N1+1)/2) - R1;
			U0 = (N1*N0) + (N0*(N0+1)/2) - R0;
			U = MIN(U1,U0);
			AUC = 1 - (U/(N1*N0));
			AUL = _EVENT_MEAN / 2 + (1-_EVENT_MEAN)*AUC;
			KI = (2*AUL-1)/(1-_EVENT_MEAN);
			OUTPUT;
		END;
	RUN;

	PROC SQL;
		DROP TABLE WORK.PROPORTION;
	QUIT;

	PROC SORT DATA = WORK.VALUES;
		BY _DATA;
	RUN;

	ODS SELECT ALL;
	PROC PRINT DATA = WORK.VALUES LABEL NOOBS;
		LABEL AUC = "AREA UNDER ROC" AUL = "AREA UNDER LIFT" KI = "ACCURACY RATIO" 
			  _DATA = "DATASET";
	RUN;
%MEND;

/* Courbe ROC */
%MACRO ROCCURVE(TABLE, 
				VARIABLESCORE, 
				VARIABLEY, 
				EVENT = 1);

	DATA WORK.ROC ;
		SET &TABLE (KEEP = &VARIABLEY &VARIABLESCORE);

	_Y = (&VARIABLEY = &EVENT);

	DO SEUIL = 0 TO 1 BY 0.01;
		VARIABLESCORE = (&VARIABLESCORE >= SEUIL);
		OUTPUT;
	END;
	RUN;

	PROC SQL ;
		CREATE TABLE WORK.ROC AS
			SELECT 	SEUIL,
					SUM(VARIABLESCORE=1 AND _Y=1)/SUM(_Y=1) AS _SENSIT_ LABEL = "SENSITIVITY",
					SUM(VARIABLESCORE=0 AND _Y=0)/SUM(_Y=0) AS _SPECIF_ LABEL = "SPECIFICITY",
					1-CALCULATED _SPECIF_ 					AS _1MSPEC_ LABEL = "1 - SPECIFICITY"
	FROM WORK.ROC
	GROUP BY SEUIL;
	QUIT;

	SYMBOL I = JOIN;
	PROC GPLOT DATA = WORK.ROC;
		PLOT _SENSIT_ * _1MSPEC_;
		FORMAT _SENSIT_ _1MSPEC_ PERCENT12.2;
	RUN; 
	QUIT;
%MEND;
/* Courbe LIFT */
%MACRO LIFTCURVE(TABLE,
				 VARIABLESCORE, 
				 VARIABLEY, 
				 EVENT = 1);

PROC SQL NOPRINT;
	SELECT ROUND(MEAN(&VARIABLEY = &EVENT) * 100,5) INTO : PCTEVENT
FROM &TABLE
WHERE &VARIABLESCORE IS NOT MISSING;
QUIT;

PROC RANK DATA = &TABLE (WHERE = (&VARIABLESCORE IS NOT MISSING)) OUT = WORK.LIFT 
																  GROUPS = 20 DESCENDING TIES = HIGH;
	VAR &VARIABLESCORE;
	RANKS QUANTILES;
RUN;

ODS EXCLUDE ALL;
ODS OUTPUT LIST = WORK.LIFTCURVE;
PROC FREQ DATA = WORK.LIFT;
	TABLE QUANTILES * &VARIABLEY / SPARSE LIST;
RUN;

PROC SQL;
	CREATE TABLE WORK.LIFT AS

		SELECT (QUANTILES + 1) * 5 AS QUANTILES,
				100 * FREQUENCY / SUM(FREQUENCY) AS PERCENT

FROM WORK.LIFTCURVE
WHERE &VARIABLEY = &EVENT;
QUIT;

ODS SELECT ALL;
DATA WORK.LIFT;
	SET WORK.LIFT (RENAME = (QUANTILES = PCT)) END = FIN;

	RETAIN LIFTMODELE;

	IF _N_ = 1 THEN LIFTMODELE = 0;
	LIFTMODELE = LIFTMODELE + PERCENT;

	MODEL = "MODEL ";
	LIFT = LIFTMODELE;
	OUTPUT;

	MODEL = "PERFECT";
	LIFT = MIN(PCT * 100 / &PCTEVENT, 100);
	OUTPUT;

	MODEL = "RANDOM ";
	LIFT = PCT;
	OUTPUT;

	IF FIN THEN DO;
		PCT = 0;
		LIFT = 0;
		MODEL = "MODEL ";
		OUTPUT;
		MODEL = "PERFECT";
		OUTPUT;
		MODEL = "RANDOM ";
		OUTPUT;
		PCT = 100;
		LIFT = 100;
		MODEL = "MODEL ";
		OUTPUT;
		MODEL = "PERFECT";
		OUTPUT;
		MODEL = "RANDOM ";
		OUTPUT;
	END;
RUN;

PROC SORT DATA = WORK.LIFT;
	BY MODEL PCT;
RUN;

SYMBOL I = JOIN;
PROC GPLOT DATA = WORK.LIFT;
	PLOT LIFT * PCT = MODEL / HAXIS = 0 TO 100 BY 20;
	LABEL LIFT = "% EVENT" PCT = "% POP ORDERED BY SCORE";
RUN;
QUIT;

%MEND;

%DIAGNOSTICS(TABLES = APP_PRED 
					  TEST_PRED, VARSCORE      = P_1, VARTARGET = RETOUR, EVENT = 1);
%ROCCURVE(   TABLE  = TEST_PRED, VARIABLESCORE = P_1, VARIABLEY = RETOUR, EVENT = 1);
%LIFTCURVE(  TABLE  = TEST_PRED, VARIABLESCORE = P_1, VARIABLEY = RETOUR, EVENT = 1);
/* La sensibilite mesure le fait de bien detecter l'evenement      : TP / (TP + FN) */
/* La specificite mesure le fait de bien detecter le non evenement : TN / (TN + FP) */

/*C'est pas mal il semble meme que les predictions se passent encore mieux sur la base de tests							