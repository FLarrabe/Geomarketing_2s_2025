SELECT 
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,
  zc.nom_provin,
  zc.urbano,

  -- Tasa de Maternidad Adolescente
  COALESCE(ROUND(
    COUNT(*) FILTER (WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19 AND p.p19 >= 1)::numeric /
    NULLIF(COUNT(*) FILTER (WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19), 0) * 100, 2), 0
  ) AS maternidad_adolescente,

  -- Tasa de Asistencia Educacional
  COALESCE(ROUND(
    COUNT(*) FILTER (WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19 AND p.p13 = 1)::numeric /
    NULLIF(COUNT(*) FILTER (WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19), 0) * 100, 2), 0
  ) AS asistencia_educacional,

  -- Población base
  COUNT(*) FILTER (WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19) AS total_mujeres_adolescentes

FROM public.personas AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna
JOIN dpa.zonas_censales_rm AS zc ON z.geocodigo::text = zc.geocodigo::text  -- AMBOS COMO TEXTO

WHERE zc.urbano = 1 
  AND (zc.nom_provin = 'SANTIAGO' OR zc.nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'))

GROUP BY z.geocodigo, c.nom_comuna, zc.nom_provin, zc.urbano
HAVING COUNT(*) FILTER (WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 19) >= 10
ORDER BY maternidad_adolescente DESC;