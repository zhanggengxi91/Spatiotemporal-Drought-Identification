% grid

function [lon,lat]=xy2lonlat(x,y)

lon=lonmin+(x-1)*0.5;
lat=latmax-(y-1)*0.5;
end