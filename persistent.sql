
create database pretest;

create user pretestuser;
alter user pretestuser with encrypted password '123@123@123';


\connect pretest

/* append only */
create table "user" (
  "timestamp" integer not null,
  seq integer not null
);

create index user_order on "user" using btree ("timestamp", seq);

/* append only */
create table "order" (
  "timestamp" integer not null,
  seq integer not null,
  orderType smallint not null,
  cardType smallint not null,
  currency smallint not null,
  amount real not null,
  ownerTimestamp integer not null,
  ownerSeq integer not null
);

create index order_order on "order"
  using btree (ownerTimestamp, ownerSeq, "timestamp", seq);

create table order_tracker (
  ownerTimestamp integer not null,
  ownerSeq integer not null,
  orderTimestamp integer not null,
  orderSeq integer not null,
  orderType smallint not null,
  cardType smallint not null,
  currency smallint not null,
  amount real not null,
  matchTimestamp integer null,
  matchOwnerTimestamp integer null,
  matchOwnerSeq integer null,
  matchOrderTimestamp integer null,
  matchOrderSeq integer null
);

create index order_tracker_order on order_tracker
  using btree (ownerTimestamp, ownerSeq, orderTimestamp, orderSeq);

create table incomplete_order (
  ownerTimestamp integer not null,
  ownerSeq integer not null,
  orderTimestamp integer not null,
  orderSeq integer not null,
  orderType smallint not null,
  cardType smallint not null,
  currency smallint not null,
  amount real not null
);

create index incomplete_order_order on incomplete_order
  using btree (ownerTimestamp, ownerSeq, orderTimestamp, orderSeq);

create sequence trade_seq;

/* append only */
create table trade (
  "timestamp" integer not null,
  seq integer not null,
  ownerTimestamp integer not null,
  ownerSeq integer not null,
  orderTimestamp integer not null,
  orderSeq integer not null,
  matchOwnerTimestamp integer not null,
  matchOwnerSeq integer not null,
  matchOrderTimestamp integer not null,
  matchOrderSeq integer not null
);

create index trade_order on trade using btree ("timestamp", seq);
create index trade_part_1_user_order on trade
  using btree (ownerTimestamp, ownerSeq);
create index trade_part_1_order_order on trade
  using btree (orderTimestamp, orderSeq);
create index trade_part_2_user_order on trade
  using btree (matchOwnerTimestamp, matchOwnerSeq);
create index trade_part_2_order_order on trade
  using btree (matchOrderTimestamp, matchOrderSeq);

create table trade_tracker (
  cardType smallint not null,
  "timestamp" integer not null,
  seq integer not null,
  ownerTimestamp integer not null,
  ownerSeq integer not null,
  orderTimestamp integer not null,
  orderSeq integer not null,
  orderType smallint not null,
  currency smallint not null,
  amount real not null,
  matchOwnerTimestamp integer not null,
  matchOwnerSeq integer not null,
  matchOrderTimestamp integer not null,
  matchOrderSeq integer not null,
  matchCurrency smallint not null,
  matchAmount real not null
);

create index trade_tracker_order on trade_tracker
  using btree ("timestamp", seq);
create index trade_tracker_part_1_user_order on trade_tracker
  using btree (ownerTimestamp, ownerSeq);
create index trade_tracker_part_1_order_order on trade_tracker
  using btree (cardType, orderTimestamp, orderSeq);
create index trade_tracker_part_2_user_order on trade_tracker
  using btree (matchOwnerTimestamp, matchOwnerSeq);
create index trade_tracker_part_2_order_order on trade_tracker
  using btree (cardType, matchOrderTimestamp, matchOrderSeq);

create table status (
  field_1 varchar(20),
  field_2 int
  );


grant all privileges on all tables in schema public to pretestuser;
