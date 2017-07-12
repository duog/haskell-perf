DROP VIEW IF EXISTS package_stats;
CREATE VIEW package_stats AS
SELECT p.name as program_name, l.id as label_id, l.name as label_name, pa.name as package_name, COUNT(*) as sample_count
, AVG(bytes_allocated) as bytes_allocated_avg
, MAX(bytes_allocated) as bytes_allocated_max
, MIN(bytes_allocated) as bytes_allocated_min
, STDEV(bytes_allocated) / AVG(bytes_allocated) as bytes_allocated_stdevpc
, AVG(num_GCs) as num_GCs_avg
, MAX(num_GCs) as num_GCs_max
, MIN(num_GCs) as num_GCs_min
, STDEV(num_GCs) / AVG(num_GCs) as num_GCs_stdevpc
, AVG(average_bytes_used) as average_bytes_used_avg
, MAX(average_bytes_used) as average_bytes_used_max
, MIN(average_bytes_used) as average_bytes_used_min
, STDEV(average_bytes_used) / AVG(average_bytes_used) as average_bytes_used_stdevpc
, AVG(max_bytes_used) as max_bytes_used_avg
, MAX(max_bytes_used) as max_bytes_used_max
, MIN(max_bytes_used) as max_bytes_used_min
, STDEV(max_bytes_used) / AVG(max_bytes_used) as max_bytes_used_stdevpc
, AVG(num_byte_usage_samples) as num_byte_usage_samples_avg
, MAX(num_byte_usage_samples) as num_byte_usage_samples_max
, MIN(num_byte_usage_samples) as num_byte_usage_samples_min
, STDEV(num_byte_usage_samples) / AVG(num_byte_usage_samples) as num_byte_usage_samples_stdevpc
, AVG(peak_megabytes_allocated) as peak_megabytes_allocated_avg
, MAX(peak_megabytes_allocated) as peak_megabytes_allocated_max
, MIN(peak_megabytes_allocated) as peak_megabytes_allocated_min
, STDEV(peak_megabytes_allocated) / AVG(peak_megabytes_allocated) as peak_megabytes_allocated_stdevpc
, AVG(init_cpu_seconds) as init_cpu_seconds_avg
, MAX(init_cpu_seconds) as init_cpu_seconds_max
, MIN(init_cpu_seconds) as init_cpu_seconds_min
, STDEV(init_cpu_seconds) / AVG(init_cpu_seconds) as init_cpu_seconds_stdevpc
, AVG(init_wall_seconds) as init_wall_seconds_avg
, MAX(init_wall_seconds) as init_wall_seconds_max
, MIN(init_wall_seconds) as init_wall_seconds_min
, STDEV(init_wall_seconds) / AVG(init_wall_seconds) as init_wall_seconds_stdevpc
, AVG(mutator_cpu_seconds) as mutator_cpu_seconds_avg
, MAX(mutator_cpu_seconds) as mutator_cpu_seconds_max
, MIN(mutator_cpu_seconds) as mutator_cpu_seconds_min
, STDEV(mutator_cpu_seconds) / AVG(mutator_cpu_seconds) as mutator_cpu_seconds_stdevpc
, AVG(mutator_wall_seconds) as mutator_wall_seconds_avg
, MAX(mutator_wall_seconds) as mutator_wall_seconds_max
, MIN(mutator_wall_seconds) as mutator_wall_seconds_min
, STDEV(mutator_wall_seconds) / AVG(mutator_wall_seconds) as mutator_wall_seconds_stdevpc
, AVG(GC_cpu_seconds) as GC_cpu_seconds_avg
, MAX(GC_cpu_seconds) as GC_cpu_seconds_max
, MIN(GC_cpu_seconds) as GC_cpu_seconds_min
, STDEV(GC_cpu_seconds) / AVG(GC_cpu_seconds) as GC_cpu_seconds_stdevpc
, AVG(GC_wall_seconds) as GC_wall_seconds_avg
, MAX(GC_wall_seconds) as GC_wall_seconds_max
, MIN(GC_wall_seconds) as GC_wall_seconds_min
, STDEV(GC_wall_seconds) / AVG(GC_wall_seconds) as GC_wall_seconds_stdevpc
, AVG(init_cpu_seconds + mutator_cpu_seconds + GC_cpu_seconds) as total_cpu_seconds_avg
, MAX(init_cpu_seconds + mutator_cpu_seconds + GC_cpu_seconds) as total_cpu_seconds_max
, MIN(init_cpu_seconds + mutator_cpu_seconds + GC_cpu_seconds) as total_cpu_seconds_min
, STDEV(init_cpu_seconds + mutator_cpu_seconds + GC_cpu_seconds) / AVG(init_cpu_seconds + mutator_cpu_seconds + GC_cpu_seconds) as total_cpu_seconds_stdevpc
, AVG(init_wall_seconds + mutator_wall_seconds + GC_wall_seconds) as total_wall_seconds_avg
, MAX(init_wall_seconds + mutator_wall_seconds + GC_wall_seconds) as total_wall_seconds_max
, MIN(init_wall_seconds + mutator_wall_seconds + GC_wall_seconds) as total_wall_seconds_min
, STDEV(init_wall_seconds + mutator_wall_seconds + GC_wall_seconds) / AVG(init_wall_seconds + mutator_wall_seconds + GC_wall_seconds) as total_wall_seconds_stdevpc
FROM
sample as s
INNER JOIN
program AS p ON s.program_id = p.id
INNER JOIN
label as l ON s.label_id = l.id
INNER JOIN
package as pa ON s.package_id = pa.id
GROUP BY program_name, label_name, package_name
;
