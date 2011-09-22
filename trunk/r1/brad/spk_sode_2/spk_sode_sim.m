filename = 'spk_sode_sim.out';
fid      = fopen(filename, 'r');
fgetl(fid)
fgetl(fid)
'Assuming the previous two lines were'
'M = 10, N = 10'
'             ti            dri            dai            dbi'
data = fscanf(fid, '%f', 4 * N * M);
data = reshape(data, 4, N, M);
for i = 1 : M
	z = data(:, :, i)';
	t = z(:,1);
	r = z(:,3);
	plot(t, r);
	hold on;
end
hold off;
fclose(fid);
