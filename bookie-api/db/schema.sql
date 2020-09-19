SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: update_timestamp(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.update_timestamp() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW;
END;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id integer NOT NULL,
    email character varying NOT NULL,
    password_hash character varying NOT NULL,
    first_name character varying NOT NULL,
    unit_size real NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    last_name character varying NOT NULL
);


--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: wager_type; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.wager_type (
    id integer NOT NULL,
    wager_type character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: wager_type_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.wager_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: wager_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.wager_type_id_seq OWNED BY public.wager_type.id;


--
-- Name: wagers; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.wagers (
    id integer NOT NULL,
    wager_type_id integer,
    wager_details jsonb,
    bettor_id integer,
    offerer_id integer,
    amount real NOT NULL,
    description character varying(140),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT self_bet CHECK ((bettor_id <> offerer_id))
);


--
-- Name: wagers_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.wagers_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: wagers_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.wagers_id_seq OWNED BY public.wagers.id;


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Name: wager_type id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.wager_type ALTER COLUMN id SET DEFAULT nextval('public.wager_type_id_seq'::regclass);


--
-- Name: wagers id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.wagers ALTER COLUMN id SET DEFAULT nextval('public.wagers_id_seq'::regclass);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: users users_email_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_email_key UNIQUE (email);


--
-- Name: users_email; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_email ON public.users USING btree (email);


--
-- Name: users_id; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_id ON public.users USING btree (id);


--
-- Name: wager_type_id; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX wager_type_id ON public.wager_type USING btree (id);


--
-- Name: wagers_id; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX wagers_id ON public.wagers USING btree (id);


--
-- Name: users users_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER users_updated_at BEFORE UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION public.update_timestamp();


--
-- Name: wagers wagers_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER wagers_updated_at BEFORE UPDATE ON public.wagers FOR EACH ROW EXECUTE FUNCTION public.update_timestamp();


--
-- Name: wagers wagers_bettor_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.wagers
    ADD CONSTRAINT wagers_bettor_id_fkey FOREIGN KEY (bettor_id) REFERENCES public.users(id);


--
-- Name: wagers wagers_offerer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.wagers
    ADD CONSTRAINT wagers_offerer_id_fkey FOREIGN KEY (offerer_id) REFERENCES public.users(id);


--
-- Name: wagers wagers_wager_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.wagers
    ADD CONSTRAINT wagers_wager_type_id_fkey FOREIGN KEY (wager_type_id) REFERENCES public.wager_type(id);


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20200104122356'),
    ('20200112080004'),
    ('20200715032739'),
    ('20200716014301'),
    ('20200716014401');
