// RUN: %neal-swift

/*binary operators*/
/*basic rules*/
a +++ b
a+++b

/*prefix unary operator*/
a(+++b);
a[+++b];
(a,+++b);
a;+++b;
(a:+++b,_:1);

/*postfix unary operator*/
a+++.b;
[a+++][b17];

(a+++,b);
a+++;b;
c ? a+++:b;

/*ternary operator*/
a ? b : c;

/*optional chaining*/
a?.b
a? .b
