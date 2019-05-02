//http://web.eecs.utk.edu/~bvz/teaching/cs461Sp11/notes/parse_tree/
%{
#include<stdio.h> // done
#include<stdlib.h> // done
#include<ctype.h> // done
#include<string.h> // done
#include "lex.yy.c" // done

void yyerror(const char*);// it is expected to accept a string to be used in the error message
int yylex();//returns a value indicating the type of token that has been obtained.
int yywrap();// Function yywrap is called by lex when input is exhausted. Return 1 if you are done or 0 if more processing is required
void insert_type();// iss function se keword waali cheeze add jhoti h symbol table me or agar pta nhi h kis type ki , n/a k liye
void add(char);// iss function se baaki cheeze aad hoti h jo abhi latest parse hui h or hm sure h ki type ki h
int sym_search(char *);
int search(char *);// symbol tree me parsed string h ki nhi pehle se uske liye h
void addTo(char i,char *n);//symbol tree me jo pehel se h unme add krne k liye jo bhi saath me argument bhejoge
void FOO();
void add_ptr();// kisi bhi data type ka pointer bna raha h
void pop();
void insert_type_table();// identifier k liye
char temptype(char *,char*);//float  h ya int yeh dekhne k liye
void type_check(char *,char*);//data type me error to nhi h , jo do bheje h vo alg to nhi h
void printtree(struct node1*);//recursion se tree print krta h

struct node{      //yeh to ek simple node h jisme sirf val h
	int val;
	} x;
struct dataType{	
	char * id_name;
	char * data_type;
	char * type;
	int line_no;
	}symbolTable[20];//yeh symbol table k structures ka array h isme saari symbol table bnegi

int ifd=0;//Label
int eld=20;
char typeStack[10][100];
int typeStack_top = 0;
char type[10];
char count=0;
int nxt=1;//printed next once
int c=0;//Temp var count
int q;
extern int countn;

struct node1{			// yeh binary tree bnaya hua h
	struct node1*left;
	struct node1*right;
	char* token;
	};
struct node1* mknode(struct node1 *left,struct node1 *right, char *token);

%}
%union { 	// to handle multiple data types 
	struct var_name {
		char name[100];
		struct node1* nd;
		} nam ; 
	struct gen_code{
		char tr[10];
		char fal[10];
		struct node1* nd;
		} gen; 
	} 
%token <nam> IF ELSE INT FLOAT CHAR  // done
%token VOID INCLUDE RETURN  // done
%token <nam> LE GE LT GT EQ NE NUM AND OR TR FL STRLT ID printff scanff // done
%type <gen> B C
%type <nam> relop Q T F E assign Arg P I M R TER N G U S EL
%start P  // whole input should match p
%left '+' '-' '*' '/'  // done
%right '=' '^' // done


%%


P : 	I // #include k liye	 
	M /*int hai yaa void ya kuch or*/ ID /*main*/ {
	insert_type_table();
	} 
	'('{
		add('t');//punctuation add krwa lia
	} R ')'
	{
		add('t');// yaha bhi
	} '{'
	{
		add('t');// yahn bhi
	} S 
	{ 
	printf("Label next:\n");
	}  U '}'
	{$$.nd = mknode(NULL,$12.nd,"start");// $$ pseudo variable hota h action or parser k communication me help krta h
	printf("#######################################################################################\n");
	printf("\t\t\tSyntax Tree in Inorder traversal\n#######################################################################################\n");
	printtree($$.nd); // tree print kra dia
	printf("\n\n");// line add krdi
	add('t');//akhiri brace ko bhi add krdia
};
I : I I | INCLUDE {add('H');} ;//include ho skta h haath k haath add bhi krwa liya 'h' bhej kr
M : INT{insert_type();} | FLOAT{insert_type();}| CHAR{insert_type();} | VOID{insert_type();} ;//int float wagarah check krlia or haath k haaath add krwa lia
R : R ','{add('t');} R | M N TER | N TER;// iska mtlb h yaa to "int a;" yaa fir "a;" 

TER : ';'{add('t');} | ; // semi colon k liye 
N : ID{insert_type_table();} G | '*'{add_ptr();} N ;  //array ya pointer k liye h
G : '['{add('t');} NUM {add('n');}']' G |  '['ID ']' G | '['{add('t');} ']' G |  ;// array ya pointer ka symbol h yeh

U : RETURN NUM {add('n');} ';'{add('t');printf("Return\t%s\n",$2.name);} | RETURN ID ';' {add('t');printf("Return\t%s\n",$2.name);}| ;

S :	IF{add('k');} '(' C ')'{printf("\nLabel\t%s:\n",$4.tr);}'{'{addTo('{',"Punctuations");} S '}'{addTo('}',"Punctuations");//andar ki body h
	pop();
	printf("goto next\n");
	printf("\nLabel\t%s:\n",$4.fal);} EL {$$.nd=mknode($4.nd,$9.nd,"IF");
	strcpy($$.name,"IF");}
	|assign {$$.nd=$1.nd;}
	|M ID TER {$$.nd=mknode(NULL,NULL,"definition"); int i=sym_search($2.name);if(i!=-1)
	{if(strcmp($1.name,"int")==0){addTo('i',$2.name);}
	else if(strcmp($1.name,"float")==0)addTo('f',$2.name);
	else addTo('c',$2.name);}
	else{printf("Variable already defined, error at line no: %d\n",yylineno);exit(0);}}
	|S S {$$.nd=mknode($1.nd,$2.nd,"statement");strcpy($$.name,"STATEMENT");}
	|printff  {add('f');} '(' STRLT ')'';' {$$.nd = mknode(NULL,NULL,"printf");}
	|scanff {add('f');}'('STRLT ',''&'ID')' ';'{$$.nd = mknode(NULL,NULL,"scanf");}
	|{$$.nd=mknode(NULL,NULL,"EPSILON");};
	
EL: ELSE{add('k');} '{'{addTo('{',"Punctuations");} S '}' {$$=$5;addTo('}',"Punctuations");//else waali conditions k liye h
	pop();
	printf("goto next\n");
	printf("\n");}| {printf("goto next\n");
	printf("\n");};

Arg : STRLT ;//koi string as a argument

C : C AND B | C OR B | NE B | B{$$.nd=$1.nd;};


B : 	E relop E {$$.nd=mknode($1.nd,$3.nd,$2.name);
   	int i=search($1.name);
   	int j=search($3.name);
   	if(i!=0&&j!=0){
   	printf("if %s %s %s goto L%d else goto L%d\n",$1.name,$2.name,$3.name,ifd,eld);
   sprintf($$.tr,"L%d",ifd);
   sprintf($$.fal,"L%d",eld);ifd++;eld++;}   
   else{printf(" Variable not declared at line no: %d\n", yylineno);exit(0);}}   
   | ID '=' {add('o');} E{int i=search($1.name);
 	int j=search($4.name);
 	if(i!=0&&j!=0)
 	{
 	printf("if %s!=0 goto L%d else goto L%d\n",$1.name,ifd,eld);
 	sprintf($$.tr,"L%d",ifd);
	sprintf($$.fal,"L%d",eld);ifd++;eld++;} 
	else{printf(" Variable not declared at line no: %d\n", yylineno);
	exit(0);}} 
	| FL{printf("if False  goto L%d\n",eld);
	sprintf($$.tr,"L%d",ifd);
	sprintf($$.fal,"L%d",eld);ifd++;eld++;} 
	| TR {printf("if True  goto L%d\n",ifd);
	sprintf($$.tr,"L%d",ifd);
	sprintf($$.fal,"L%d",eld);ifd++;eld++;} 
	| ID {int i=search($1.name);
	if(i!=0)
	{
	printf("if %s!=0  goto L%d else goto L%d\n",$1.name,ifd,eld);
	sprintf($$.tr,"L%d",ifd);
	sprintf($$.fal,"L%d",eld);ifd++;eld++;}    
	else {printf(" Variable not declared at line no: %d\n", yylineno);
	exit(0);} }
	| NUM {add('n');
	printf("if %s!=0  goto L%d else goto L%d\n",$1.name,ifd,eld);
	sprintf($$.tr,"L%d",ifd);
	sprintf($$.fal,"L%d",eld);ifd++;eld++;} ;

assign :  ID '='{add('o');} E ';'{$1.nd = mknode(NULL,NULL,$1.name);
	  $$.nd=mknode($1.nd,$4.nd,"=");
	  strcpy($$.name,"=");add('t');
	  int i=search($1.name);
	  int j=search($4.name);
	  if(i!=0&&j!=0) 
	  {
	  type_check($1.name,$4.name);
	  printf("= \t %s\t %s \n",$4.name,$1.name);} 
	  else {printf("Variable not declared at line no: %d\n", yylineno);
	  exit(0);}} |ID '(' Arg ')' ';'{add('t');};

E :	E '+'{add('o');} E {$$.nd=mknode($1.nd,$4.nd,"+");strcpy($$.name,"+");
	int i=search($1.name);
	int j=search($4.name);
	sprintf($$.name,"t%d",c);c++;
	addTo(temptype($1.name,$4.name),$$.name);
	if(i!=0 && j!=0) {printf("%s\t%s\t%s\t%s\n","+",$1.name,$4.name,$$.name);}
	 else {printf(" Variable not declared at line no: %d\n", yylineno);exit(0);}}


	| E '-' {add('o');}E { $$.nd=mknode($1.nd,$4.nd,"-");
	strcpy($$.name,"-");
	int i=search($1.name);
	int j=search($4.name);
	sprintf($$.name,"t%d",c);c++;
	addTo(temptype($1.name,$4.name),$$.name);
	if(i!=0 && j!=0) {printf("%s\t%s\t%s\t%s\n","-",$1.name,$4.name,$$.name);} 
	else {printf(" Variable not declared at line no: %d\n", yylineno);exit(0);}}
	| F{$$.nd=$1.nd;};
	
F :	 F '*'{add('o');} F {$$.nd=mknode($1.nd,$4.nd,"*");
	 strcpy($$.name,"*");
	 int i=search($1.name);
	 int j=search($4.name);
	 sprintf($$.name,"t%d",c);c++;
	 addTo(temptype($1.name,$4.name),$$.name);
	 if(i!=0 && j!=0) {printf("%s\t%s\t%s\t%s\n","*",$1.name,$4.name,$$.name);}
	 else {printf(" Variable not declared at line no: %d\n", yylineno);exit(0);}}

	| F '/' {add('o');}F {$$.nd=mknode($1.nd,$4.nd,"/");strcpy($$.name,"/"); 
	int i=search($1.name);
	int j=search($4.name);
	sprintf($$.name,"%d",c);
	strcat($$.name,"t");c++;
	addTo(temptype($1.name,$4.name),$$.name);
	if(i!=0 && j!=0) 
	{
	printf("%s\t%s\t%s\t%s\n","/",$1.name,$4.name,$$.name);
	}
	 else {printf(" Variable not declared at line no: %d\n", yylineno);
	 exit(0);}}

	| T {$$.nd=$1.nd;};
	
T :	T '^'{add('o');} T {$$.nd=mknode($1.nd,$4.nd,"^");
	strcpy($$.name,"^");
	int i=search($1.name);
	int j=search($4.name);
	sprintf($$.name,"%d",c);
	strcat($$.name,"t");c++;
	addTo(temptype($1.name,$4.name),$$.name);
	if(i!=0 && j!=0) {
	printf("%s\t%s\t%s\t%s\n","^",$1.name,$4.name,$$.name);} 
	else {printf(" Variable not declared at line no: %d\n", yylineno);exit(0);}}

	| Q {$$.nd=$1.nd;};
	
Q : '('{add('t');} E ')'{add('t'); $$=$3;} | ID {insert_type_table();} G {$$.nd=mknode(NULL,NULL,$1.name);strcpy($$.name,$1.name);}| NUM{add('n');}{$$.nd=mknode(NULL,NULL,$1.name);
	strcpy($$.name,$1.name);} ;


relop : LE {add('r');} | GE {add('r');}| LT {add('r');} | GT {add('r');}| EQ {add('r');};



%%
int main()
{
	extern int yylineno;
	x.val=10;
	printf("#######################################################################################\n");
	printf("\t\t\tIntermediate code\n");
	printf("#######################################################################################\n");
	yyparse();
	
	printf("\nParsing is Successful\n");	
	printf("#######################################################################################\n");
	printf("\t\t\tSymbol table\n");
	printf("#######################################################################################\n");	
	printf("\nsymbol \t type  \t identify \t line number\n");
	printf("_______________________________________________________________________________________\n");
	int i=0;
	for(i=0;i<count;i++){
		printf("%s\t%s\t%s\t%d\t\n",symbolTable[i].id_name,symbolTable[i].data_type,symbolTable[i].type,symbolTable[i].line_no);
		
	}
	printf("_______________________________________________________________________________________\n");
	printf("#######################################################################################\n");
	printf("\t\t\t\t  Project By:\n");
	printf("\t\t\t\t______________\n");
	printf("\n\t\t\tShorya Khanna \t 16103298 \n\t\t\tPalak Arora \t 16103046 \n\t\t\tAman Parmar \t 16103221 \n");
	printf("_______________________________________________________________________________________\n");
		printf("#######################################################################################\n");
	for(i=0;i<count;i++){
		free(symbolTable[i].id_name);//strdup function se jo memory dynamically allocate hui thi usse free krra h
		free(symbolTable[i].type);//esa krna hi pdta h
	}
	
	return 0;
}
void yyerror(const char* s)
{
	printf("Not accepted\n");
	exit(0);
}
//iss function se symbol table me symbol insert hore h mtlb jo array of structures h ussme
void insert_type(){

	
	strcpy(type,yytext);
	q=search(type);// symbol table me pehle se to nhi h string to mtlb koi keyword hoga
	if(q==0){
		
		symbolTable[count].id_name=strdup(yytext);
		symbolTable[count].data_type=strdup("N/A");
		symbolTable[count].line_no = countn;
		symbolTable[count].type=strdup("KEYWORD\t");
		count++;
	}
	
	
}
//add declaration of data to symboltable
void addTo(char i,char *n)
{
	if(i=='i')
	{
			symbolTable[count].id_name=strdup(n);
			symbolTable[count].data_type="int";
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("variable");
			count++;
	}
	else if(i=='f')
	{
			symbolTable[count].id_name=strdup(n);
			symbolTable[count].data_type="float";
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("variable");
			count++;
	}
	else if(i=='c')
	{
			symbolTable[count].id_name=strdup(n);
			symbolTable[count].data_type="char";
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("variable");
			count++;
	}
	else if(i=='{')
	{
			symbolTable[count].id_name=strdup("{");;
			symbolTable[count].data_type="N/A";
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("punctuation");
			count++;
	}
	else if(i=='}')
	{
			symbolTable[count].id_name=strdup("}");;
			symbolTable[count].data_type="N/A";
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("punctuation");
			count++;
	}


}
//yeh function node bnanata h 
struct node1* mknode(struct node1 *left, struct node1 *right, char *token)
{
	
  struct node1 *newnode = (struct node1 *)malloc(sizeof(struct node1));
  char *newstr = (char *)malloc(strlen(token)+1);
  strcpy(newstr, token);
  newnode->left = left;
  newnode->right = right;
  newnode->token = newstr;
  return(newnode);
}
void pop()
{
	int i;
	//printf("count %d\n",count);
	int temp=count-1;
	for(i=temp;i>=0;i--)
	{
		if(strcmp(symbolTable[i].id_name,"{")!=0)
		{
			//printf("$$\n");
			count=count-1;;
		}
		else
		{
			count=count-1;
			break;
		}
	}	
	

}

char temptype(char* one,char* two)
{
		int y;
	char* onetype;
	char* twotype;
	for(y = 0;y<count;y++)
	{
		if(strcmp(symbolTable[y].id_name,one)==0) onetype=symbolTable[y].data_type;
		if(strcmp(symbolTable[y].id_name,two)==0) twotype=symbolTable[y].data_type;	
	}
	if((strcmp(onetype,"float")==0) || (strcmp(twotype,"float")==0))
		return 'f';
	else
		return 'i';
}
void insert_type_table(){
	
		q=search(yytext);
	
		if(q==0){
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup(type);
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("IDENTIFIER");
			count++;
		}
	
	
}
void type_check(char* one, char* two)
{
	int y;
	char* onetype;
	char* twotype;
	for(y = 0;y<count;y++)
	{
		if(strcmp(symbolTable[y].id_name,one)==0) onetype=symbolTable[y].data_type;
		if(strcmp(symbolTable[y].id_name,two)==0) twotype=symbolTable[y].data_type;	
	}
	if(strcmp(onetype,twotype)>0){ printf("type error at lineno %d\n",yylineno);exit(0);}
}
//ADD the recent parsed string into symboltable
void add(char c)
{
	q=search(yytext);
	if(q==0){
		if(c=='H')
		{
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup(type);
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("Header");
			count++;
		}
		else if(c=='t')
		{	
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("N/A");
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("Punctuation");
			count++;
		}
		else if(c=='o')
		{
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("N/A");
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("Operator");
			count++;
		}
		else if(c=='r')
		{
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("N/A");
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("Rel Op\t");
			count++;
		}
		else if(c=='k')
		{
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("N/A");
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("KEYWORD\t");
			count++;
		}
		else if(c=='n')
		{
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("int");
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("NUMBER\t");
			count++;
		}
	else if(c=='f')
		{
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("N/A");
			symbolTable[count].line_no = countn;
			symbolTable[count].type=strdup("FUNCTION\t");
			count++;
		}
	}
}
int  sym_search(char *type)
{
	int i;
	for(i=count -1 ;i>=0&&(strcmp(symbolTable[i].id_name,"{")!=0);i--)
	{
		if(strcmp(symbolTable[i].id_name,type)==0)
		{
			return -1;
			break;
		}
	
	}
	return 0;
}
//Search in the Symbol table whether the parsed String is present in Symbol table already
int  search(char *type)
{
	int i;
	for(i=count -1 ;i>=0;i--)
	{
		if(strcmp(symbolTable[i].id_name,type)==0)
		{
			return -1;
			break;
		}
	
	}
	return 0;
}
void add_ptr(){
	strcat(type,"*");
}
//print the abstract syntax tree
void printtree(struct node1* tree)
{
  int i;
  if (tree->left)
	{
 	printtree(tree->left);
	}
  printf(" %s , ", tree->token);
  if (tree->right)
	{
   	printtree(tree->right);
	}
 
}
