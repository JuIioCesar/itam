create database itamtesis;

create table noticias(
 fecha timestamp,
 url character varying,
 title character varying,
 description character varying,
 content character varying,
 origen character varying,
 seccion character varying
);

create table clasificaciones(
  content character varying,
  tag_bm25 character varying,
  bm25 numeric(10,2),
  tag_tfidf character varying,
  tfidf numeric(10,2)
);

create index idx_noticias_url on noticias(url);
create index idx_noticias_title on noticias(title);
create index idx_noticias_fecha on noticias(fecha);
create index idx_noticias_origen on noticias(origen);

--seccion nacional
update noticias set seccion = 'nacional'
  from
(select url from noticias
where url like '%/nacional/%'
and origen like '%financiero') as a
where noticias.url = a.url;

--seccion mundo
update noticias set seccion = 'mundo'
  from
(select url from noticias
where url like '%/mundo/%'
and origen like '%financiero') as a
where noticias.url = a.url;

--seccion financial-times
update noticias set seccion = 'financial_times'
  from
(select url from noticias
where url like '%/financial-times/%'
and origen like '%financiero') as a
where noticias.url = a.url;

--seccion tech
update noticias set seccion = 'tech'
  from
(select url from noticias
where url like '%/tech/%'
and origen like '%financiero') as a
where noticias.url = a.url;

---agregando la seccion y url a las clasificaciones
with a as(
  select title||' '||description||' '||content as contenido, url, seccion
  from noticias
)

update clasificaciones
  set url = a.url,
  seccion = a.seccion
  from (select title||' '||description||' '||content as contenido, url, seccion
  from noticias) as a
  where content = contenido;
