-- migrate:up
create table wager_type
(
  id            serial,
  wager_type    varchar not null,
  created_at    timestamptz not null default now(),
  updated_at    timestamptz not null default now()
);

create unique index wager_type_id on wager_type (id);

insert into wager_type
  (wager_type)
values
  ('Moneyline'),
  ('Over Under'),
  ('Spread')

-- migrate:down
drop table wager_type;

