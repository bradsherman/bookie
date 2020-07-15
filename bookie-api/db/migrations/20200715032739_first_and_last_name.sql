-- migrate:up
alter table users
rename column name to first_name;

alter table users
add column last_name varchar;

update users
set last_name = split_part(first_name, ' ', 2);

alter table users
alter column last_name set not null;

-- migrate:down
alter table users
drop column last_name;

alter table users
rename column first_name to name;
