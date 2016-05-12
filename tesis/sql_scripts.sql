create database itamtesis;

create table noticias(
 fecha timestamp,
 url character varying,
 title character varying,
 description character varying,
 content character varying,
 origen character varying
);

create table clasificaciones(
  content character varying,
  tags character varying ARRAY[5]
);

create index idx_noticias_url on noticias(url);
create index idx_noticias_title on noticias(title);
create index idx_noticias_fecha on noticias(fecha);
create index idx_noticias_origen on noticias(origen);
