use v6;

# Based on the perl 5 module of the same name. All mistakes are mine.

use Geo::Geometry;

# The following code was extracted into a separate module, but has now
# been incorporated back here due to name conflistc.

# use Geo::Ellipsoid;

class Ellipsoid {

has Str $.name                   is required;
has Real $.semi-major-axis       is required;
has Real $.semi-minor-axis;
has Real $.eccentricity-squared;
has Real $.inverse-flattening;

sub prefix:<√> { sqrt($^a) }

submethod TWEAK {
    fail "Specify semi-major-axis and exactly one of semi-minor-access, eccentricity-squared or inverse-flattening"
      unless $!semi-minor-axis.defined
      xor    $!inverse-flattening.defined
      xor    $!eccentricity-squared.defined;
    
    if $!inverse-flattening.defined {
        $!eccentricity-squared = 2 ÷ $!inverse-flattening - 1÷$!inverse-flattening²;
        $!semi-minor-axis = $!semi-major-axis × (1 - 1 ÷ $!inverse-flattening);
    } elsif $!eccentricity-squared.defined { # e² is defined
        $!inverse-flattening = 1 ÷ (1 - √(1 - $!eccentricity-squared));
        $!semi-minor-axis = $!semi-major-axis × (1 - 1 ÷ $!inverse-flattening);
    } else { # semi-minor-axis defined
        $!inverse-flattening = $!semi-major-axis ÷ ($!semi-major-axis - $!semi-minor-axis);
        $!eccentricity-squared = 2 ÷ $!inverse-flattening - 1 ÷ $!inverse-flattening²;
    }
}

our @Ellipsoid =
    ( Ellipsoid.new(name => "Airy",                                semi-major-axis => 6377563.396, semi-minor-axis      => 6356256.909),   # eccentricity-squared => 0.006670540
      Ellipsoid.new(name => "Arc 1950",                            semi-major-axis => 6378249.145, eccentricity-squared => 0.006803481),
      Ellipsoid.new(name => "Australian National",                 semi-major-axis => 6378160,     inverse-flattening   => 298.25),        # eccentricity-squared => 0.006694542
      Ellipsoid.new(name => "Bessel 1841",                         semi-major-axis => 6377397.155, semi-minor-axis      => 6356078.963),   # eccentricity-squared => 0.006674372
      Ellipsoid.new(name => "Bessel 1841 Nambia",                  semi-major-axis => 6377484,     eccentricity-squared => 0.006674372),   # same as non-Nambia, but slightly different sized metre
      Ellipsoid.new(name => "Clarke 1866",                         semi-major-axis => 6378206.4,   semi-minor-axis      => 6356583.8),     # eccentricity-squared => 0.006768658
      Ellipsoid.new(name => "Clarke 1880",                         semi-major-axis => 6378249.145, inverse-flattening   => 293.465),       # eccentricity-squared => 0.006803511
      Ellipsoid.new(name => "Everest 1830 India",                  semi-major-axis => 6377276,     eccentricity-squared => 0.006637847),
      Ellipsoid.new(name => "Everest 1830 Malaysia",               semi-major-axis => 6377299,     eccentricity-squared => 0.006637847),
      Ellipsoid.new(name => "Everest 1956 India",                  semi-major-axis => 6377301,     eccentricity-squared => 0.006637847),
      Ellipsoid.new(name => "Everest 1964 Malaysia and Singapore", semi-major-axis => 6377304,     eccentricity-squared => 0.006637847),
      Ellipsoid.new(name => "Everest 1969 Malaysia",               semi-major-axis => 6377296,     eccentricity-squared => 0.006637847),
      Ellipsoid.new(name => "Everest Pakistan",                    semi-major-axis => 6377296,     eccentricity-squared => 0.006637534),
      Ellipsoid.new(name => "Fischer 1960 Mercury",                semi-major-axis => 6378166,     eccentricity-squared => 0.006693422),
      Ellipsoid.new(name => "Fischer 1968",                        semi-major-axis => 6378150,     eccentricity-squared => 0.006693422),
      Ellipsoid.new(name => "GRS 1967",                            semi-major-axis => 6378160,     eccentricity-squared => 0.006694605),
      Ellipsoid.new(name => "GRS 1980",                            semi-major-axis => 6378137,     eccentricity-squared => 0.006694380),
      Ellipsoid.new(name => "Helmert 1906",                        semi-major-axis => 6378200,     eccentricity-squared => 0.006693422),
      Ellipsoid.new(name => "Hough",                               semi-major-axis => 6378270,     eccentricity-squared => 0.006722670),
      Ellipsoid.new(name => "Indonesian 1974",                     semi-major-axis => 6378160,     eccentricity-squared => 0.006694609),
      Ellipsoid.new(name => "International",                       semi-major-axis => 6378388,     inverse-flattening   => 297),           # eccentricity-squared => 0.006722670
      Ellipsoid.new(name => "Krassovsky",                          semi-major-axis => 6378245,     eccentricity-squared => 0.006693422),
      Ellipsoid.new(name => "Modified Airy",                       semi-major-axis => 6377340,     eccentricity-squared => 0.006670540),
      Ellipsoid.new(name => "Modified Everest",                    semi-major-axis => 6377304,     eccentricity-squared => 0.006637847),
      Ellipsoid.new(name => "Modified Fischer 1960",               semi-major-axis => 6378155,     eccentricity-squared => 0.006693422),
      Ellipsoid.new(name => "NAD 27",                              semi-major-axis => 6378206.4,   eccentricity-squared => 0.006768658),
      Ellipsoid.new(name => "NAD 83",                              semi-major-axis => 6378137,     eccentricity-squared => 0.006694384),
      Ellipsoid.new(name => "South American 1969",                 semi-major-axis => 6378160,     eccentricity-squared => 0.006694542),
      Ellipsoid.new(name => "WGS 60",                              semi-major-axis => 6378165,     eccentricity-squared => 0.006693422),
      Ellipsoid.new(name => "WGS 66",                              semi-major-axis => 6378145,     inverse-flattening   => 298.25),        # eccentricity-squared => 0.006694542
      Ellipsoid.new(name => "WGS-72",                              semi-major-axis => 6378135,     inverse-flattening   => 298.26),        # eccentricity-squared => 0.006694318
      Ellipsoid.new(name => "WGS-84",                              semi-major-axis => 6378137,     inverse-flattening   => 298.257223562), # eccentricity-squared => 0.00669438
    );

our %Ellipsoid = @Ellipsoid.map({.name => $_});

# remove all markup from an ellipsoid name, to increase the chance
# that a match is found.
our sub cleanup-ellipsoid-name(Str $copy is copy) is export {
    $copy .= lc;
    $copy ~~ s:g/ \( <-[)]>* \) //;   # remove text between parentheses
    $copy ~~ s:g/ <[\s-]> //;         # no blanks or dashes
    $copy;
}

for @Ellipsoid {
    %Ellipsoid{cleanup-ellipsoid-name($_.name)} = $_;
}

# Returns all pre-defined ellipsoid names, sorted alphabetically
sub ellipsoid-names() is export {
    @Ellipsoid.map({ .name }).sort;
}

}

module Geo::Coordinates::UTM {

class PointUTM {
    has Str $.zone;
    has Num $.easting;
    has Num $.northing;

    method List {
        ($!zone, $!easting, $!northing);
    }
}

class LineStringUTM {
    has PointUTM @.points;
}

class LinearRingUTM {
    has PointUTM @.points;
}

class PolygonUTM {
    has LinearRingUTM @.rings;
}

class PointLatLon {
    has $.x;
    has $.y;

    method List { ($!y, $!x) }
    method Point { Point.new(x => $!x, y => $!y) }
}

constant \deg2rad = π ÷ 180;
constant \rad2deg = 180 ÷ π;

sub valid-utm-zone(Str $char) {
    state $valid-zones = set <C D E F G H J K L M N P Q R S T U V W X>;
    $char ∈ $valid-zones;
}

sub latlon-zone-number(Real $latitude, Real $long2) {
    my $zone = ( ($long2 + 180) ÷ 6 ).Int + 1;
    if 56 ≤ $latitude < 64.0 && 3.0 ≤ $long2 < 12.0 {
        $zone = 32;
    }
    if 72 ≤ $latitude < 84.0 { 
        $zone =  ( 0.0 ≤ $long2 <  9.0) ?? 31
   	!! ( 9.0 ≤ $long2 < 21.0) ?? 33
   	!! (21.0 ≤ $long2 < 33.0) ?? 35
        !! (33.0 ≤ $long2 < 42.0) ?? 37
   	!!                           $zone;
    }
    $zone;
}

# The following variables are used as a cache; their values do not
# change for calculations on a particular ellipsoid
my $zone-name;
my $zone-number;
my $name;
my $eccentricity;
my $radius;
my $eccentprime;
my $longorigin;
my $longoriginradian;
my ($k1, $k2, $k3, $k4);

my constant $k0 = 0.9996e0;     # scale factor

sub get-ellipsoid($name) {
    %Ellipsoid::Ellipsoid{$name} // %Ellipsoid::Ellipsoid{Ellipsoid::cleanup-ellipsoid-name($name)};
}

sub set-ellipsoid($ellipsoid) {
    state $lastellipsoid = '';
    return if $ellipsoid eq $lastellipsoid;
    
    $lastellipsoid = $ellipsoid;

    my $ell = get-ellipsoid($ellipsoid);
    
    fail "Ellipsoid $ellipsoid unknown." unless $ell;
    
    $name         = $ell.name;
    $radius       = $ell.semi-major-axis;
    $eccentricity = $ell.eccentricity-squared;

    $eccentprime      = $eccentricity ÷ (1 - $eccentricity);
    $k1 = $radius × ( 1 - $eccentricity ÷ 4
                      - 3 × $eccentricity × $eccentricity ÷ 64
                      - 5 × $eccentricity × $eccentricity × $eccentricity ÷ 256);
    $k2 =  $radius × (3 × $eccentricity ÷ 8
                      +  3 × $eccentricity × $eccentricity ÷ 32
                      + 45 × $eccentricity × $eccentricity × $eccentricity ÷ 1024);
    $k3 = $radius × (15 × $eccentricity × $eccentricity ÷ 256
                     + 45 × $eccentricity × $eccentricity × $eccentricity ÷ 1024);
    $k4 = $radius × (35 × $eccentricity × $eccentricity × $eccentricity ÷ 3072);
}

multi sub set-ellipsoid-zone(Str $ellipsoid, Str $zone) {
    set-ellipsoid $ellipsoid;
    
    $zone ~~ m:i/ ^ (\d+) <[CDEFGHJKLMNPQRSTUVWX]> ? $ /;
    $zone-name = $zone;
    $zone-number = ~$/[0];
    
    fail "Zone value ($zone) invalid." unless $zone-number.defined && $zone-number <= 60;

    $longorigin       = ($zone-number - 1)×6 - 180 + 3;
    $longoriginradian = deg2rad × $longorigin;
}

multi sub set-ellipsoid-zone(Str $ellipsoid, Real $latitude, Real $longitude) {
    set-ellipsoid $ellipsoid;
    
    my $long2         = $longitude - (($longitude + 180)÷360).Int × 360;
    $zone-number      = latlon-zone-number($latitude, $long2); 
    $zone-name        = $zone-number.Str;

    $longorigin       = ($zone-number - 1)×6 - 180 + 3;
    $longoriginradian = deg2rad × $longorigin;
}

# Raw underlying routine
#
# Expects ellipsoid and zone to have been previously set.
# Returns PointUTΜ object

sub latlon2utm(Real $latitude, Real $longitude) {
    my $long2 = $longitude - (($longitude + 180)÷360).Int × 360;
    
    my $lat-radian       = deg2rad × $latitude;
    my $long-radian      = deg2rad × $long2;
    
    my $N = $radius ÷ sqrt(1-$eccentricity × sin($lat-radian)×sin($lat-radian));
    my $T = tan($lat-radian) × tan($lat-radian);
    my $C = $eccentprime × cos($lat-radian)×cos($lat-radian);
    my $A = cos($lat-radian) × ($long-radian - $longoriginradian);
    my $M = $k1 × $lat-radian
          - $k2 × sin(2 × $lat-radian)
          + $k3 × sin(4 × $lat-radian)
          - $k4 × sin(6 × $lat-radian);
    
    my $utm-easting  = $k0×$N×($A+(1-$T+$C)×$A³/6
                                 +(5-18×$T+$T²+72×$C-58×$eccentprime)×$A⁵/120)
                       + 500000.0;
    
    my $utm-northing = $k0 × ( $M
                               + $N × tan($lat-radian)
                               × ( $A²÷2
                                   + (5 - $T + 9×$C + 4×$C²)×$A⁴÷24
                                   + (61 - 58×$T + $T² + 600×$C - 330×$eccentprime) × $A⁶÷720
                                 )
                             );
    
    $utm-northing += 10000000.0 if $latitude < 0;
    
    my $utm-letter
         =  ( 84 ≥ $latitude ≥  72) ?? 'X'
         !! ( 72 > $latitude ≥  64) ?? 'W'
         !! ( 64 > $latitude ≥  56) ?? 'V'
         !! ( 56 > $latitude ≥  48) ?? 'U'
         !! ( 48 > $latitude ≥  40) ?? 'T'
         !! ( 40 > $latitude ≥  32) ?? 'S'
         !! ( 32 > $latitude ≥  24) ?? 'R'
         !! ( 24 > $latitude ≥  16) ?? 'Q'
         !! ( 16 > $latitude ≥   8) ?? 'P'
         !! (  8 > $latitude ≥   0) ?? 'N'
         !! (  0 > $latitude ≥  -8) ?? 'M'
         !! ( -8 > $latitude ≥ -16) ?? 'L'
         !! (-16 > $latitude ≥ -24) ?? 'K'
         !! (-24 > $latitude ≥ -32) ?? 'J'
         !! (-32 > $latitude ≥ -40) ?? 'H'
         !! (-40 > $latitude ≥ -48) ?? 'G'
         !! (-48 > $latitude ≥ -56) ?? 'F'
         !! (-56 > $latitude ≥ -64) ?? 'E'
         !! (-64 > $latitude ≥ -72) ?? 'D'
         !! (-72 > $latitude ≥ -80) ?? 'C'
         !! fail "Latitude ($latitude) out of UTM range.";
    
    PointUTM.new(zone     => $zone-name ~ $utm-letter,
                 easting  => $utm-easting,
                 northing => $utm-northing);
}

# Expects Ellipsoid Number or name, Latitude, Longitude 
# (Latitude and Longitude in decimal degrees)
# Returns UTM Zone, UTM Easting, UTM Northing

multi sub latlon-to-utm(Str $ellipsoid, $point where Point|PointZ|PointM|PointZM, Str :$zone) is export {
    if $zone.defined {
        set-ellipsoid-zone($ellipsoid, $zone)
    } else {
        set-ellipsoid-zone($ellipsoid,
                           $point.y,
                           $point.x
                          );
    }
    latlon2utm($point.y, $point.x);
}

multi sub latlon-to-utm(Str $ellipsoid, $line where LineString|LineStringZ|LineStringM|LineStringZM, Str :$zone is copy) is export {
    if $zone.defined {
        set-ellipsoid-zone($ellipsoid, $zone)
    } else {
        set-ellipsoid-zone($ellipsoid,
                           $line.points[0].y,
                           $line.points[0].x
                          );
    }
    
    LineStringUTM.new($line.points.map({.latlon2utm(.y, .x)}));
}

multi sub latlon-to-utm(Str $ellipsoid, $polygon where Polygon|PolygonZ|PolygonM|PolygonZM, Str :$zone is copy) is export {
    if $zone.defined {
        set-ellipsoid-zone($ellipsoid, $zone)
    } else {
        set-ellipsoid-zone($ellipsoid,
                           $polygon.rings[0].points[0].y,
                           $polygon.rings[0].points[0].x
                          );
    }
    
    PolygonUTM.new( rings =>
                    $polygon.rings.map(
                          {LinearRingUTM.new(points => .points.map(
                                                    {.latlon2utm($_.y, $_.x)}
                                                ))
                          }
                      )
                  );
}

multi sub latlon-to-utm(Str $ellipsoid, Real $latitude, Real $longitude, Str :$zone is copy) is export {
    fail "Longitude value ($longitude) invalid."
    unless -180 ≤ $longitude ≤ 180;
    
    if $zone.defined {
        set-ellipsoid-zone($ellipsoid, $zone)
    } else {
        set-ellipsoid-zone($ellipsoid, $latitude, $longitude);
    }
    
    my $utm = latlon2utm($latitude, $longitude);
    ($utm.zone, $utm.easting, $utm.northing);
}

# Expects Ellipsoid Number or name, UTM zone, UTM Easting, UTM Northing
# (in various different forms)
# Returns Latitude, Longitude in form appropriate to input

multi sub utm-to-latlon(Str $ellipsoid, PointUTM $utm) is export {
    my ($latitude, $longitude) = | utm-to-latlon($ellipsoid, $utm.zone, $utm.easting, $utm.northing);
    PointLatLon.new(x => $longitude, y => $latitude);
}

multi sub utm-to-latlon(Str $ellipsoid, LineStringUTM $utm) is export {
    LineString.new(points => $utm.points.map({utm-to-latlon($ellipsoid, .zone, .easting, .northing)}));
}

multi sub utm-to-latlon(Str $ellipsoid, PolygonUTM $utm) is export {
    Polygon.new( rings =>
                 $utm.rings.map(
                       {LinearRing.new(points => .points.map(
                                              {utm-to-latlon($ellipsoid, .zone, .easting, .northing)}
                                     ))
                       }
                   )
               );
}

# The following routine should ellipsoid-invariant calculations cached

multi sub utm-to-latlon(Str $ellipsoid, Str $zone, Real $easting, Real $northing) is export {
    my $ell = get-ellipsoid($ellipsoid);
    fail "Unknown ellipsoid $ellipsoid." unless $ell;

    my $radius          = $ell.semi-major-axis;
    my $eccentricity    = $ell.eccentricity-squared;

    $zone              ~~ /^(\d+)(.*)/;
    my $zone-number     = $0;
    my Str $zone-letter = $1.Str.uc;

    fail "UTM zone ($zone-letter) invalid." unless valid-utm-zone $zone-letter;

    my $x  = $easting - 500000; # Remove Longitude offset
    my $y  = $northing;
       $y -= 10000000.0 if $zone-letter lt 'N'; # Remove Southern Offset

    my $longorigin      = ($zone-number - 1)×6 - 180 + 3;
    my $eccPrimeSquared = ($eccentricity)÷(1-$eccentricity);
    my $M               = $y÷$k0;
    my $mu              = $M÷($radius×(1-$eccentricity÷4-3×$eccentricity²÷64-5×$eccentricity³÷256));

    my $e1              = (1-sqrt(1-$eccentricity))÷(1+sqrt(1-$eccentricity));
    my $phi1rad         = $mu+(3×$e1÷2-27×$e1³÷32)×sin(2×$mu)+(21×$e1²÷16-55×$e1⁴÷32)×sin(4×$mu)+(151×$e1³÷96)×sin(6×$mu);
    my $phi1            = $phi1rad×rad2deg;
    my $N1              = $radius÷sqrt(1-$eccentricity×sin($phi1rad)²);
    my $T1              = tan($phi1rad)²;
    my $C1              = $eccentricity×cos($phi1rad)²;
    my $R1              = $radius × (1-$eccentricity) ÷ ((1-$eccentricity×sin($phi1rad)²)**1.5);
    my $D               = $x÷($N1×$k0);

    my $Latitude = $phi1rad - ( $N1 × tan($phi1rad) ÷ $R1 )
                              × ( $D² ÷ 2 - (5 + 3 × $T1 + 10 × $C1
                                               - 4 × $C1² - 9 × $eccPrimeSquared
                                            ) × $D⁴ ÷ 24
                                          + (61 +  90 × $T1
                                                + 298 × $C1
                                                +  45 × $T1²
                                                - 252 × $eccPrimeSquared
                                                -   3 × $C1²)
                                            × $D⁶ ÷ 720
                              );
    $Latitude ×= rad2deg;

    my $Longitude = ($D
                     - (1 + 2 × $T1 + $C1) × $D³ ÷ 6
                     + (5
                        - 2 × $C1
                        + 28 × $T1
                        - 3 × $C1²
                        + 8 × $eccPrimeSquared
                        + 24 × $T1²
                       ) × $D⁵ ÷ 120
                    ) ÷ cos($phi1rad);
    $Longitude = $longorigin + $Longitude × rad2deg;

    ($Latitude, $Longitude);
}

sub utm-to-mgrs(Str $zone, Real $easting, Real $northing) is export {
    my $zone-number     = $zone;
    my Str $zone-letter = $zone-number;
    $zone-number       ~~ s/^(\d+)(.*)//;
    $zone-number        = +$0;
    $zone-letter        = ~$1;

    fail "UTM zone ($zone-letter) invalid."
    unless valid-utm-zone $zone-letter;

    my $northing-zones = "ABCDEFGHJKLMNPQRSTUV";
    my $rnd-north      = sprintf("%d",$northing);
    my $north-split    = $rnd-north.chars - 5;
       $north-split    = 0 if $north-split < 0;
    my $mgrs-north     = $rnd-north.substr($rnd-north.chars-5);
       $rnd-north     -= 2000000 while ($rnd-north >= 2000000);
       $rnd-north     += 2000000 if $rnd-north < 0;
    my $num-north      = ($rnd-north÷100000).Int;
       $num-north     += 5 if not ($zone-number % 2);
       $num-north     -= 20 until $num-north < 20;
    my $lett-north     = $northing-zones.substr($num-north,1);
    my $rnd-east       = sprintf("%d",$easting);
    my $east-split     = $rnd-east.chars-5;
       $east-split     = 0 if $east-split < 0;
    my $mgrs-east      = $rnd-east.substr($rnd-east.chars-5, Inf);
    my $num-east       = $rnd-east.substr(0, $rnd-east.chars-5);
       $num-east       = 0 if not $num-east;
    my $mgrs-zone      = $zone-number;
       $mgrs-zone     -= 3 until $mgrs-zone < 4;
    # zones are 6deg wide, mgrs letters are 18deg = 8 per zone
    # calculate which zone required
    my $easting-zones
      =  ( $mgrs-zone == 1) ?? 'ABCDEFGH'
        !! ( $mgrs-zone == 2) ?? 'JKLMNPQR'
        !! ( $mgrs-zone == 3) ?? 'STUVWXYZ'
        !! fail "Could not calculate MGRS zone.";
       $num-east--;
    my $lett-east      = $easting-zones.substr($num-east,1) or fail "Could not detect Easting Zone for MGRS coordinate";

    "$zone$lett-east$lett-north$mgrs-east$mgrs-north";
}

sub latlon-to-mgrs(Str $ellipsoid, Real $latitude, Real $longitude) is export {
    my ($zone,$x-coord,$y-coord) = |latlon-to-utm($ellipsoid, $latitude, $longitude);
    utm-to-mgrs($zone,$x-coord,$y-coord);
}

sub mgrs-to-utm(Str $mgrs-string is copy) is export {
    state $valid-letters1 = set <A B C D E F G H J K L M N P Q R S T U V W X Y Z>;
    state $valid-letters2 = set <A B C D E F G H J K L M N P Q R S T U V>;
    
    # ensure two digits in zone number
    my $zone            = $mgrs-string.substr(0,2);
    $mgrs-string        = '0' ~ $mgrs-string if $zone !~~ /^\d+$/;
    $zone               = $mgrs-string.substr(0,3);

    my $zone-number     = $zone;
    my Str $zone-letter = $zone-number;
    $zone-number       ~~ s/^(\d+)(.*)//;
    $zone-number        = +$0;
    $zone-letter        = ~$1;

    fail "UTM zone ($zone-letter) invalid."
    unless valid-utm-zone $zone-letter;

    my $first-letter = $mgrs-string.substr(3,1);
    fail "MGRS zone ($first-letter) invalid." unless $first-letter ∈ $valid-letters1;

    my $second-letter = $mgrs-string.substr(4,1);
    fail "MGRS zone ($second-letter) invalid." unless $second-letter ∈ $valid-letters2;

    my $coords    = $mgrs-string.substr(5, Inf);
    my $coord-len = $coords.chars;
    fail "MGRS coords ($coords) invalid." unless (0 < $coord-len <= 10) and !($coord-len % 2);

    $coord-len  = ($coord-len÷2).Int;
    my $x-coord = $coords.substr(0,$coord-len);
    my $y-coord = $coords.substr($coord-len, Inf);
    $x-coord   ×= 10 ** (5 - $coord-len);
    $y-coord   ×= 10 ** (5 - $coord-len);

    my $east-pos
    =  ( $first-letter ~~ /<[ABCDEFGH]>/) ?? index('ABCDEFGH',$first-letter)
    !! ( $first-letter ~~ /<[JKLMNPQR]>/) ?? index('JKLMNPQR',$first-letter)
    !! ( $first-letter ~~ /<[STUVWXYZ]>/) ?? index('STUVWXYZ',$first-letter)
    !! fail "Could not calculate MGRS Easting zone.";
    fail "MGRS Letter $first-letter invalid." if $east-pos < 0;
    $east-pos++;
    $east-pos ×= 100000;
    $x-coord  += $east-pos;

    my $northing-zones = "ABCDEFGHJKLMNPQRSTUV";
    my $north-pos      = $northing-zones.index($second-letter);
    fail "MGRS Letter $second-letter invalid." if $north-pos < 0;
    $north-pos++;
    $north-pos -= 5 if not ($zone-number % 2);
    $north-pos += 20 until $north-pos > 0;
    if ($zone-letter ~~ /<[NPQRSTUVWX]>/) { # Northern hemisphere
        my $tmpNorth = index('NPQRSTUVWX',$zone-letter);
        $tmpNorth++;
        $tmpNorth    ×= 8;
        $tmpNorth    ×= 10÷9;
        $tmpNorth     = ((($tmpNorth - $north-pos)÷20)+0.5).Int*20;
        $north-pos   += $tmpNorth;
        $north-pos   ×= 100000;
        $north-pos   -= 100000;
        $y-coord     += $north-pos;
    }
    else { # Southern Hemisphere
        my $tmpNorth = index('CDEFGHJKLM',$zone-letter);
        $tmpNorth    ×= 8;
        $tmpNorth    ×= 10÷9;
        $tmpNorth     = ((($tmpNorth-$north-pos)÷20)+0.5).Int*20;
        $north-pos   += $tmpNorth;
        $north-pos   ×= 100000;
        $north-pos   -= 100000;
        $north-pos   += 2000000 if $zone-letter ne "C";
        $y-coord     += $north-pos;
    }

    ($zone,$x-coord,$y-coord);
}

sub mgrs-to-latlon(Str $ellipsoid, Str $mgrs-string) is export {
    my ($zone,$x-coord,$y-coord) = |mgrs-to-utm($mgrs-string);
    utm-to-latlon($ellipsoid,$zone,$x-coord,$y-coord);
}

}

=begin pod
=head1 NAME

Geo::Coordinates::UTM - Perl extension for Latitude Longitude conversions.

=head1 SYNOPSIS

use Geo::Coordinates;
use Geo::Coordinates::UTM;

my ($zone,$easting,$northing)= |latlon-to-utm($ellipsoid,$latitude,$longitude);

my $utmpoint = latlon-to-utm($ellipsoid, $latitude, $longitude);

my $point = Point.new(x => 141.5, y => -23.78);

$utmpoint = latlon-to-utm($ellipsoid, $point); # returns PointUTM

my $polygon = Polygon.new(...);

my $polygon-utm = latlon-to-utm($ellipsoid, $polygon); # returns PolygonUTM

$point = utm-to-latlon($ellipsoid, $zone, $easting, $northing); # returns PointLatLon

$point = utm-to-latlon($ellipsoid, $zone, $easting, $northing).Point; # coerce to Point

my ($latitude,$longitude)= |utm-to-latlon($ellipsoid,$zone,$easting,$northing);

my ($latitude,$longitude)= |utm-to-latlon($ellipsoid, $utmpoint);

my ($zone,$easting,$northing)= |mgrs-to-utm($mgrs);

my ($latitude,$longitude)= |mgrs-to-latlon($ellipsoid,$mgrs);

my ($mgrs)= |utm-to-mgrs($zone,$easting,$northing);

my ($mgrs)= |latlon-to-mgrs($ellipsoid,$latitude,$longitude);

=head1 DESCRIPTION

This module will translate latitude longitude coordinates
to Universal Transverse Mercator(UTM) coordinates and vice versa.

=head2 Mercator Projection

The Mercator projection was first invented to help mariners. They needed
to be able to draw a straight line on a map and follow that bearing to
arrive at a destination. In order to do this,
Mercator invented a projection which preserved angle, by projecting the
earth's surface onto a cylinder, sharing the same axis as the earth
itself. This caused all Latitude and Longitude lines to be straight and
to intersect at a 90° angle, but the downside was that the scale of
the map increased as you moved away from the equator so that the lines of
longitude were parallel.

Because the scale varies, areas near the poles appear much larger on the
map than a similar sized object near the equator. The Mercator Projection
is useless near the poles since the scale becomes infinite.

=head2 Transverse Mercator Projection

A Transverse Mercator projection takes the cylinder and turns it on its
side. Now the cylinder's axis passes through the equator, and it can be
rotated to line up with the area of interest. Many countries use
Transverse Mercator for their grid systems. The disadvantage is that now
neither the lines of latitude or longitude (apart from the central
meridian) are straight.

=head2 Universal Transverse Mercator

The Universal Transverse Mercator(UTM) system sets up a universal world
wide system for mapping. The Transverse Mercator projection is used,
with the cylinder in 60 positions. This creates 60 zones around the
world. Positions are measured using Eastings and Northings, measured in
meters, instead of Latitude and Longitude. Eastings start at 500,000 on
the centre line of each zone. In the Northern Hemisphere, Northings are
zero at the equator and increase northward. In the Southern Hemisphere,
Northings start at 10 million at the equator, and decrease southward.
You must know which hemisphere and zone you are in to interpret your
location globally. Distortion of scale, distance and area increase away
from the central meridian.

UTM projection is used to define horizontal positions world-wide by
dividing the surface of the Earth into 6° zones, each mapped by
the Transverse Mercator projection with a central meridian in the center
of the zone. UTM zone numbers designate 6° longitudinal strips
extending from 80° South latitude to 84° North latitude.
UTM zone characters designate 8° zones extending north and south
from the equator. Eastings are measured from the central meridian (with
a 500 km false easting to insure positive coordinates). Northings are
measured from the equator (with a 10,000 km false northing for positions
south of the equator).

UTM is applied separately to the Northern and Southern Hemisphere, thus
within a single UTM zone, a single X / Y pair of values will occur in
both the Northern and Southern Hemisphere. To eliminate this confusion,
and to speed location of points, a UTM zone is sometimes subdivided into
20 zones of Latitude. These grids can be further subdivided into 100,000
meter grid squares with double-letter designations. This subdivision by
Latitude and further division into grid squares is generally referred to
as the Military Grid Reference System (MGRS). The unit of measurement of
UTM is always meters and the zones are numbered from 1 to 60 eastward,
beginning at the 180th meridian. The scale distortion in a north-south
direction parallel to the central meridian (CM) is constant However, the
scale distortion increases either direction away from the CM. To
equalize the distortion of the map across the UTM zone, a scale factor
of 0.9996 is applied to all distance measurements within the zone. The
distortion at the zone boundary, 3°s away from the CM is
approximately 1%.

=head2 Ellipsoids

Ellipsoids are imported from Geo::Ellipsoids.

=head2 latlon-to-utm

C<latlon-to-utm> is the main routine for converting
latitude and longitude to UTΜcoordinates.
It has various incantations depending on the form of the input,
but the UTM coordinates are always expressed as a C<PointUTM> object,
or a collection of C<PointUTM> objects (C<LineStringUTM> or C<PolygonUTM>).
   
A C<PointUTM>  has three accessor methods: C<zone>, C<easting>,
and C<northing>.
In addition, and mainly for backwards compatibility with older versions
of this module, a method C<List> is provided so that the
return value of the latlon-to-utm method can be used as a list of
zone, easting and northing.

C<latlon-to-utm> can be called with explicit values
for latitude and longitude:

    my $utm = latlon-to-utm($ellipsoid, $latitude, $longitude);

with the position expressed as a C<Geo::Geometry> point type
(C<Point>, C<PointZ>, C<PointM>, or C<PointZM>):

    my $position = Point.new($longitude, %latitude_;
    my $utm = latlon-to-utm($ellipsoid, $position);

with multiple positions expressed as a C<Geo::Geometry> line type
(C<LineString>, C<LineStringZ>, C<LineStringM>, or C<LineStringZM>):

    my $line = LineString.new(...);
    my $utm = latlon-to-utm($ellipsoid, $line);

or with multiple positions expressed as a C<Geo::Geometry> polygon type
(C<Polygon>, C<PolygonZ>, C<PolygonM>, or C<PolygonZM>):

    my $polygon = Polygon.new(...);
    my $utm = latlon-to-utm($ellipsoid, $polygon);

In the latter two cases, each point within the line or polygon
will be converted to a C<PointUTM> object, and the returned value
will be a C<LineStringUTM> or C<PolygonUTM> object as appropriate.

A C<LineStringUTM> object provides a single accessor method C<points>
which returns an array of C<PointUTM> objects.
A C<PolygonUTM> object provides a single accessor method C<rings>
which returns an array of C<LinearRingUTM> objects representing
the polygon's external boundary and any internal holes.
A C<LinearRingUTM> object provides a single accessor method C<points>
which returns an array of C<PointUTM> objects.

Latitude values in the southern hemisphere should be supplied
as negative values (e.g. 30° South will be -30).
Similarly Longitude values West of the meridian should also be
supplied as negative values. Both latitude and longitude should
not be entered as deg,min,sec but as their decimal equivalent,
e.g. 30°12'22.432" sec should be entered as 30.2062311

Previous versions of this module allowed the ellipsoid to be expressed
as an index into the array of ellipsoids. This usage was marked as deprecated,
and is now removed.

For latitude  57°49'59.000" North
    longitude 02°47'20.226" West

using Clarke 1866 (Ellipsoid 5)

     ($zone,$east,$north)= |latlon-to-utm('clarke 1866',57.803055556,-2.788951667)

returns 

     $zone  = "30V"
     $east  = 512543.777159849
     $north = 6406592.20049111

On occasions, it is necessary to map a pair of (latitude, longitude)
coordinates to a predefined zone. This is done by providing a value
for the optional named parameter zone as follows:

     ($zone, $east, $north)= |latlon-to-utm('international', :zone($zone-number),
                                          $latitude, $longitude)

For instance, Spain territory goes over zones 29, 30 and 31 but
sometimes it is convenient to use the projection corresponding to zone
30 for all the country.

Santiago de Compostela is at 42°52'57.06" North, 8°32'28.70" West

    ($zone, $east, $north)= |latlon-to-utm('international',  42.882517, -8.541306)

returns

     $zone = "29T"
     $east = 537460.331
     $north = 4747955.991

but forcing the conversion to zone 30:

    ($zone, $east, $north)= |latlon-to-utm('international', :zone(30),
                                         42.882517, -8.541306)

returns

    $zone = "30T"
    $east = 47404.442
    $north = 4762771.704

This is also necessary when the eastern boundary of a map lies along
a zone boundary. If the zone is not forced for values on that boundary
then the returned values will not be useful.

=head2 utm-to-latlon

Reversing the above example,

     ($latitude,$longitude)= |utm-to-latlon(5,'30V',512543.777159849,6406592.20049111)

returns

     $latitude  = 57.8030555601332
     $longitude = -2.7889516669741

     which equates to

     latitude  57°49'59.000" North
     longitude 02°47'20.226" West

To allow compatibility with previous versions of this module, C<utm-to-latlon>
returns an object of type C<PointLatLon> which contains only the two attributes
C<x> (the longitude) and C<y> (the latitude). If used as in the above example,
the List method will be called which will return a list of the latitude and
longitude. There is also a C<Point> method to allow coercion to a more standard
type.

=head2 latlon-to-mgrs

C<latlon-to-mgrs> is called in the same way as C<latlon-to-utm>,
but instead of a C<PointUTM> object, the returned value will be
a string contain the MGRS representation of the location.

For latitude  57°49'59.000" North
    longitude 02°47'20.226" West

using WGS84 (Ellipsoid 23)

     $mgrs = latlon-to-mgrs('WGS-84', 57.8030590197684, -2.788956799)

returns 

     $mgrs  = 30VWK1254306804

Currently only the version explicitly specifying longitude and latitude
is implemented; Other versions will follow.
                
Note that previous versions of this module returned a list containing
a single value. This is no longer the case and might cause problems
in some circumstances.

=head2 mgrs-to-latlon

Reversing the above example,

     ($latitude,$longitude)=|mgrs-to-latlon(23,'30VWK1254306804')

returns

     $latitude  = 57.8030590197684
     $longitude = -2.788956799645

Like utm-to-latlon, the returned object is C<PointLatLon>.
                             
=head2 mgrs-to-utm

    Similarly it is possible to convert MGRS directly to UTM

        ($zone,$easting,$northing)=|mgrs-to-utm('30VWK1254306804')

    returns

        $zone = 30V
        $easting = 512543
        $northing = 6406804

=head2 utm-to-mgrs

and the inverse converting from UTM to MGRS is done as follows

    ($mgrs)=|utm-to-mgrs('30V',512543,6406804);

returns
    $mgrs = 30VWK1254306804

=head1 AUTHOR

Kevin Pye, kjpye@cpan.org

Graham Crookham, grahamc@cpan.org

=head1 THANKS

Thanks go to the following:

Felipe Mendonca Pimenta for helping out with the Southern
hemisphere testing.

Michael Slater for discovering the Escape \Q bug.

Mark Overmeer for the ellipsoid-info routines and code review.

Lok Yan for the >72deg. N bug.

Salvador Fandino for the forced zone UTM and additional tests

Matthias Lendholt for modifications to MGRS calculations

Peder Stray for the short MGRS patch



=head1 COPYRIGHT

Copyright (c) 2000,2002,2004,2007,2010,2013 by Graham Crookham.
All rights reserved.

copyright (c) 2018, 2022 by Kevin Pye.
    
This package is free software; you can redistribute it
and/or modify it under the same terms as Raku itself.

=end pod
