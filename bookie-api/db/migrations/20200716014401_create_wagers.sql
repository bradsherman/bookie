-- migrate:up
create table wagers
(
  id            serial,
  wager_type_id integer,
  wager_details jsonb,
  bettor_id     integer,
  offerer_id    integer,
  amount        real not null,
  description   varchar(140),
  created_at    timestamptz not null default now(),
  updated_at    timestamptz not null default now(),

  foreign key (wager_type_id) references wager_type(id),
  foreign key (bettor_id) references users(id),
  foreign key (offerer_id) references users(id),
  constraint self_bet check (bettor_id != offerer_id)
);

create unique index wagers_id on wagers (id);

create trigger wagers_updated_at
  before update on wagers
  for each row
execute procedure update_timestamp()

-- migrate:down
drop table wagers;
