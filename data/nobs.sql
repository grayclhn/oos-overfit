drop table if exists nobs;
create table nobs (i integer primary key, n integer, nlabel text, kNull integer, nulllabel text, kAlt integer, altlabel text);
insert into nobs (n, nlabel, kNull, nulllabel, kAlt, altlabel) values ( 100,  "T=100",  2, "K0=2",      3, "K=3");
insert into nobs (n, nlabel, kNull, nulllabel, kAlt, altlabel) values ( 250,  "T=250",  2, "K0=2",      3, "K=3");
insert into nobs (n, nlabel, kNull, nulllabel, kAlt, altlabel) values ( 500,  "T=500",  2, "K0=2",      3, "K=3");
insert into nobs (n, nlabel, kNull, nulllabel, kAlt, altlabel) values ( 100,  "T=100",  2, "K0=n/50",  10, "K=T/10");
insert into nobs (n, nlabel, kNull, nulllabel, kAlt, altlabel) values ( 250,  "T=250",  5, "K0=n/50",  25, "K=T/10");
insert into nobs (n, nlabel, kNull, nulllabel, kAlt, altlabel) values ( 500,  "T=500", 10, "K0=n/50",  50, "K=T/10");
