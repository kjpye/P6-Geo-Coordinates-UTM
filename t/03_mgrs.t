#!/usr/bin/perl

use Test;

plan 117;

# BEGIN { @*INC.push('../lib'); }
use Geo::Coordinates::UTM;

sub fleq ($a, $b, Real $eps = 1e-2) {
    if (abs($a - $b) < $eps) {
        return True;
    }
    return False;
}

my $latlon = "CCDEFGHJKLMNPQRSTUVWXX";

my $data = '# ellipsoid|latitude|longitude|mgrs
WGS-84|-15.038207087342|-169.633239244409|2LPJ4692336993
Airy|-62.664347298066|-18.1318011641218|27EXL4689749077
Bessel 1841 Nambia|-25.9668774017417|176.847283481794|60JVS8471328217
Fischer 1968|35.6355896701843|-128.779407270838|9SWV1997243564
Clarke 1880|-23.7249091815666|-58.725260492694|21KUP2412875382
Clarke 1880|32.1214082107167|39.4139996279968|37SER3905453697
Everest 1830 India|-77.8630147119741|-167.673126297052|3CVP3729056475
Airy|18.3773457007429|166.226582243424|58QFF2956632235
Australian National|56.0384628238617|-162.332085699794|3VXC6620413592
GRS 1967|73.8433062387819|84.4955735592175|45XVB2223396218
WGS-84|45.3359588450991|40.0400031298722|37TEL8148720798
Helmert 1906|59.9201815839611|-99.2827685844408|14VMM8418942625
Bessel 1841 Nambia |-14.7469801908782|85.8375051526079|45LUD7488269470
WGS 66|76.7872465101191|65.0798587586657|41XNF5305723979
Clarke 1880|-77.955182771371|144.231341269374|55CDP3552145300
WGS-84|-46.0648299617229|79.4920266271394|44GLP8337197644
Fischer 1968|-46.0648299617229|79.4920266271394|44GLP8337097630
Airy|-29.748187686171|-135.405807319046|8JMN6076509275
Airy|7.02664660839798|25.1759536336006|35NKH9852877035
GRS 1980|-23.328081037079|-157.911520393938|4KFV1128219742
Krassovsky|11.2850029025532|160.123416303008|57PXN2261647750
Modified Fischer 1960|63.3429435572545|-65.5753780930164|20VLR7110826412
Hough|18.8829467124758|-72.6426446845981|18QYF4834589517
Bessel 1841 Nambia |-33.5599610482547|149.259628455931|55HGC0974084663
Fischer 1968|-42.9864757600699|73.2901330922691|43GCN6059939255
GRS 1967|19.5586150524789|78.6857081619374|44QKG5719264288
International|52.870630356142|-150.706593044311|5UPU5436760466
WGS 66|59.0836358798848|-60.7869841003324|20VPL2681951474
Krassovsky|50.3319354501851|50.6262418599614|39UVR7339875702
Hough|-67.0761049801843|17.6673796933028|33DXF1591557596
Hough|44.6070598377999|-79.0353060704053|17TPK5590741178
WGS 66|56.9743083375776|-0.986959536978418|30VXJ2236216335
WGS-84|54.8376254894191|-30.4398113516179|25UFA6441379726
Everest 1830 India|7.80734863804564|-94.7379708763827|15NUJ0838563327
WGS-84|42.1388081786232|94.1239212872368|46TEM9287865799
Modified Fischer 1960|-50.2668337384696|137.577966166824|53FPE8371928502
Modified Fischer 1960|-16.0823420814511|-30.9872437684145|25KGC1529720901
Airy|-66.2328424771405|-54.6449818303734|21DXG0588052719
Everest 1830 India|-6.84757411014701|39.7458086007605|37MEN8239243096
';

for $data.lines -> $line {
    next if $line ~~ /^ \s* '#' /;


    my ($ellipsoid, $latitude, $longitude, $mgrs) = $line.split('|');
    my ($m) = latlon_to_mgrs($ellipsoid.Str, $latitude.Real, $longitude.Real);
    ok $m eq $mgrs, "MGRS $mgrs";

    my ($lat, $lon) = mgrs_to_latlon($ellipsoid, $m);
    ok fleq($lon, $longitude), "longitude $longitude";
    ok fleq($lat, $latitude),  "latitude $latitude";
}

