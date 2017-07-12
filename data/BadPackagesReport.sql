SELECT b.package_name, b.label_name
FROM
 (SELECT p.id as package_id, COUNT(*) AS c
  FROM package p
  LEFT OUTER JOIN
  sample s ON p.id = s.package_id
  WHERE s.program_id = :program_id
  GROUP BY p.name
  HAVING c > 0
 ) a
 INNER JOIN
 (SELECT p.id AS package_id, l.id, p.name AS package_name, l.name AS label_name, COUNT(*) AS C
  FROM package p
  LEFT OUTER JOIN
  sample s ON p.id = s.package_id
  LEFT OUTER JOIN
  label l ON l.id = s.label_id
  WHERE s.program_id = :program_id
  GROUP BY p.id, l.id, package_name, label_name
  HAVING c = 0
  ) b ON a.package_id = b.package_id
