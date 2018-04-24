use v6;

# Based on the perl 5 module of the same name. All mistakes are mine.

module Geo::Coordinates::UTM {

   constant \deg2rad =  pi / 180;
   constant \rad2deg = 180 /  pi;
   
   # remove all markup from an ellipsoid name, to increase the chance
   # that a match is found.
   sub cleanup-name(Str $copy is copy) {
       $copy .= lc;
       $copy ~~ s:g/ \( <-[)]>* \) //;   # remove text between parentheses
       $copy ~~ s:g/ <[\s-]> //;         # no blanks or dashes
       $copy;
   }
   
   # Ellipsoid array (name,equatorial radius,square of eccentricity)
   # Same data also as hash with key eq name (in variations)
   
   my (@Ellipsoid, %Ellipsoid);
   
   BEGIN {  # Initialize this before other modules get a chance
      @Ellipsoid =
       ( [ "Airy",                                6377563,     0.006670540]
       , [ "Australian National",                 6378160,     0.006694542]
       , [ "Bessel 1841",                         6377397,     0.006674372]
       , [ "Bessel 1841 Nambia",                  6377484,     0.006674372]
       , [ "Clarke 1866",                         6378206,     0.006768658]
       , [ "Clarke 1880",                         6378249,     0.006803511]
       , [ "Everest 1830 India",                  6377276,     0.006637847]
       , [ "Fischer 1960 Mercury",                6378166,     0.006693422]
       , [ "Fischer 1968",                        6378150,     0.006693422]
       , [ "GRS 1967",                            6378160,     0.006694605]
       , [ "GRS 1980",                            6378137,     0.006694380]
       , [ "Helmert 1906",                        6378200,     0.006693422]
       , [ "Hough",                               6378270,     0.006722670]
       , [ "International",                       6378388,     0.006722670]
       , [ "Krassovsky",                          6378245,     0.006693422]
       , [ "Modified Airy",                       6377340,     0.006670540]
       , [ "Modified Everest",                    6377304,     0.006637847]
       , [ "Modified Fischer 1960",               6378155,     0.006693422]
       , [ "South American 1969",                 6378160,     0.006694542]
       , [ "WGS 60",                              6378165,     0.006693422]
       , [ "WGS 66",                              6378145,     0.006694542]
       , [ "WGS-72",                              6378135,     0.006694318]
       , [ "WGS-84",                              6378137,     0.00669438 ]
       , [ "Everest 1830 Malaysia",               6377299,     0.006637847]
       , [ "Everest 1956 India",                  6377301,     0.006637847]
       , [ "Everest 1964 Malaysia and Singapore", 6377304,     0.006637847]
       , [ "Everest 1969 Malaysia",               6377296,     0.006637847]
       , [ "Everest Pakistan",                    6377296,     0.006637534]
       , [ "Indonesian 1974",                     6378160,     0.006694609]
       , [ "Arc 1950",                            6378249.145, 0.006803481]
       , [ "NAD 27",                              6378206.4,   0.006768658]
       , [ "NAD 83",                              6378137,     0.006694384]
       );

   # calc ecc  as  
   # a = semi major axis
   # b = semi minor axis
   # e^2 = (a^2-b^2)/a^2	
   # For clarke 1880 (Arc1950) a=6378249.145 b=6356514.966398753
   # e^2 (40682062155693.23 - 40405282518051.34) / 40682062155693.23
   # e^2 = 0.0068034810178165


     for @Ellipsoid -> $el {
         my (Str $name, Real $eqrad, Real $eccsq) = @$el;
         %Ellipsoid{$name} = $el;
         %Ellipsoid{cleanup-name $name} = $el;
     }
   }

   sub valid-utm-zone(Str $char) {
       ? $char ~~ /<[CDEFGHJKLMNPQRSTUVWX]>/;
   }

   # Returns all pre-defined ellipsoid names, sorted alphabetically
   sub ellipsoid-names() is export {
       @Ellipsoid ==> map { .[0] };
   }

   # Returns "official" name, equator radius and square eccentricity
   # The specified name can be numeric (for compatibility reasons) or
   # a more-or-less exact name
   # Examples:   my($name, $r, $sqecc) = ellipsoid-info 'wgs84';
   #             my($name, $r, $sqecc) = ellipsoid-info 'WGS 84';
   #             my($name, $r, $sqecc) = ellipsoid-info 'WGS-84';
   #             my($name, $r, $sqecc) = ellipsoid-info 'WGS-84 (new specs)';
   #             my($name, $r, $sqecc) = ellipsoid-info 22;

   sub ellipsoid-info(Str $id) is export {
       my $el = $id !~~ m/\D/
              ?? @Ellipsoid[$id-1]   # old system counted from 1
              !! %Ellipsoid{$id} || %Ellipsoid{cleanup-name $id};

       $el.defined ?? @$el !! ();
   }

   sub latlon-zone-number(Real $latitude, Real $long2) {
       my $zone = ( ($long2 + 180) / 6 ).Int + 1;
       if 56 <= $latitude < 64.0 && 3.0 <= $long2 < 12.0 {
           $zone = 32;
       }
       if 72 <= $latitude < 84.0 { 
           $zone =  ( 0.0 <= $long2 <  9.0) ?? 31
   	         !! ( 9.0 <= $long2 < 21.0) ?? 33
   	         !! (21.0 <= $long2 < 33.0) ?? 35
                 !! (33.0 <= $long2 < 42.0) ?? 37
   		 !!                            $zone;
       }
       $zone;
   }

   my $lastellips = '';
   my $name;
   my $eccentricity;
   my $radius;
   my $eccentprime;
   my ($k1, $k2, $k3, $k4);

   # Expects Ellipsoid Number or name, Latitude, Longitude 
   # (Latitude and Longitude in decimal degrees)
   # Returns UTM Zone, UTM Easting, UTM Northing

   sub latlon-to-utm(Str $ellips, Real $latitude, Real $longitude, Str :$zone is copy) is export {
       fail "Longitude value ($longitude) invalid."
           unless -180 <= $longitude <= 180;

       my $long2 = $longitude - (($longitude + 180)/360).Int * 360;
       my $zone-number;

       if $zone.defined {
           $zone ~~ m:i/ ^ (\d+) <[CDEFGHJKLMNPQRSTUVWX]> ? $ /;
           $zone-number = ~$/[0];

           fail "Zone value ($zone) invalid."
               unless $zone-number.defined && $zone-number <= 60;
       } else {
           $zone-number  = latlon-zone-number($latitude, $long2); 
           $zone         = $zone-number.Str;
       }

       if $ellips ne $lastellips { # cache the variables which don't change with the same ellipse
           $lastellips = $ellips;
           ($name, $radius, $eccentricity) = |ellipsoid-info $ellips
               or fail "Ellipsoid value ($ellips) invalid.";
           $eccentprime      = $eccentricity / (1 - $eccentricity);
           $k1 = $radius * ( 1 - $eccentricity/4
                   - 3 * $eccentricity * $eccentricity / 64
                   - 5 * $eccentricity * $eccentricity * $eccentricity/ 256);
           $k2 =  $radius * (3 * $eccentricity / 8
               +  3 * $eccentricity * $eccentricity / 32
               + 45 * $eccentricity * $eccentricity * $eccentricity / 1024);
           $k3 = $radius * (15 * $eccentricity * $eccentricity / 256
               + 45 * $eccentricity * $eccentricity * $eccentricity / 1024);
           $k4 = $radius * (35 * $eccentricity * $eccentricity * $eccentricity / 3072);
       }

       my $lat-radian       = deg2rad * $latitude;
       my $long-radian      = deg2rad * $long2;

       my $k0               = 0.9996;                  # scale

       my $longorigin       = ($zone-number - 1)*6 - 180 + 3;
       my $longoriginradian = deg2rad * $longorigin;

       my $N = $radius / sqrt(1-$eccentricity * sin($lat-radian)*sin($lat-radian));
       my $T = tan($lat-radian) * tan($lat-radian);
       my $C = $eccentprime * cos($lat-radian)*cos($lat-radian);
       my $A = cos($lat-radian) * ($long-radian - $longoriginradian);
       my $M = $k1 * $lat-radian
             - $k2 * sin(2 * $lat-radian)
             + $k3 * sin(4 * $lat-radian)
             - $k4 * sin(6 * $lat-radian);

       my $utm-easting  = $k0*$N*($A+(1-$T+$C)*$A*$A*$A/6
                        + (5-18*$T+$T*$T+72*$C-58*$eccentprime)*$A*$A*$A*$A*$A/120)
                        + 500000.0;
   
       my $utm-northing = $k0 * ( $M
                                  + $N * tan($lat-radian)
                                       * ( $A*$A/2
                                         + (5 - $T + 9*$C + 4*$C*$C)*$A*$A*$A*$A/24
                                         + (61 - 58*$T + $T*$T + 600*$C - 330*$eccentprime) * $A*$A*$A*$A*$A*$A/720
                                         )
                                );
   
       $utm-northing += 10000000.0 if $latitude < 0;
   
       my $utm-letter
         =  ( 84 >= $latitude >=  72) ?? 'X'
         !! ( 72 >  $latitude >=  64) ?? 'W'
         !! ( 64 >  $latitude >=  56) ?? 'V'
         !! ( 56 >  $latitude >=  48) ?? 'U'
         !! ( 48 >  $latitude >=  40) ?? 'T'
         !! ( 40 >  $latitude >=  32) ?? 'S'
         !! ( 32 >  $latitude >=  24) ?? 'R'
         !! ( 24 >  $latitude >=  16) ?? 'Q'
         !! ( 16 >  $latitude >=   8) ?? 'P'
         !! (  8 >  $latitude >=   0) ?? 'N'
         !! (  0 >  $latitude >=  -8) ?? 'M'
         !! ( -8 >  $latitude >= -16) ?? 'L'
         !! (-16 >  $latitude >= -24) ?? 'K'
         !! (-24 >  $latitude >= -32) ?? 'J'
         !! (-32 >  $latitude >= -40) ?? 'H'
         !! (-40 >  $latitude >= -48) ?? 'G'
         !! (-48 >  $latitude >= -56) ?? 'F'
         !! (-56 >  $latitude >= -64) ?? 'E'
         !! (-64 >  $latitude >= -72) ?? 'D'
         !! (-72 >  $latitude >= -80) ?? 'C'
         !! fail "Latitude ($latitude) out of UTM range.";
   
       $zone ~= $utm-letter;

       ($zone, $utm-easting, $utm-northing);
   }

   # Expects Ellipsoid Number or name, UTM zone, UTM Easting, UTM Northing
   # Returns Latitude, Longitude
   # (Latitude and Longitude in decimal degrees, UTM Zone e.g. 23S)

   sub utm-to-latlon(Str $ellips, Str $zone, Real $easting, Real $northing) is export {
       my ($name, $radius, $eccentricity) = |ellipsoid-info $ellips
           or fail "Ellipsoid value ($ellips) invalid.";

       $zone              ~~ /^(\d+)(.*)/;
       my $zone-number     = $0;
       my Str $zone-letter = $1.Str.uc;

       fail "UTM zone ($zone-letter) invalid."
          unless valid-utm-zone $zone-letter;

       my $k0 = 0.9996;
       my $x  = $easting - 500000; # Remove Longitude offset
       my $y  = $northing;
       $y    -= 10000000.0 if $zone-letter lt 'N'; # Remove Southern Offset

       my $longorigin      = ($zone-number - 1)*6 - 180 + 3;
       my $eccPrimeSquared = ($eccentricity)/(1-$eccentricity);
       my $M               = $y/$k0;
       my $mu              = $M/($radius*(1-$eccentricity/4-3*$eccentricity*$eccentricity/64-5*$eccentricity*$eccentricity*$eccentricity/256));

       my $e1              = (1-sqrt(1-$eccentricity))/(1+sqrt(1-$eccentricity));
       my $phi1rad         = $mu+(3*$e1/2-27*$e1*$e1*$e1/32)*sin(2*$mu)+(21*$e1*$e1/16-55*$e1*$e1*$e1*$e1/32)*sin(4*$mu)+(151*$e1*$e1*$e1/96)*sin(6*$mu);
       my $phi1            = $phi1rad*rad2deg;
       my $N1              = $radius/sqrt(1-$eccentricity*sin($phi1rad)*sin($phi1rad));
       my $T1              = tan($phi1rad)*tan($phi1rad);
       my $C1              = $eccentricity*cos($phi1rad)*cos($phi1rad);
       my $R1              = $radius * (1-$eccentricity)
                             / ((1-$eccentricity*sin($phi1rad)*sin($phi1rad))**1.5);
       my $D               = $x/($N1*$k0);

       my $Latitude = $phi1rad
                    - ( $N1 * tan($phi1rad) / $R1 )
                     *( $D * $D / 2 - (5 + 3 * $T1 + 10 * $C1
                                    - 4 * $C1 * $C1 - 9 * $eccPrimeSquared) * $D * $D * $D * $D / 24
                                    + (61 + 90 * $T1 + 298 * $C1 + 45 * $T1 * $T1 - 252 * $eccPrimeSquared - 3 * $C1 * $C1)
                                      * $D * $D * $D * $D * $D * $D / 720
                      );
          $Latitude = $Latitude * rad2deg;

       my $Longitude = ($D
                       - (1 + 2 * $T1 + $C1) * $D*$D*$D / 6
                       + (5
                         - 2 * $C1
                         + 28 * $T1
                         - 3 * $C1 * $C1
                         + 8 * $eccPrimeSquared
                         + 24 * $T1*$T1
                         ) * $D*$D*$D*$D*$D / 120
                       ) / cos($phi1rad);
          $Longitude = $longorigin + $Longitude * rad2deg;

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
      my $num-north      = ($rnd-north/100000).Int;
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
      my $lett-east      = $easting-zones.substr($num-east,1)
                         or fail "Could not detect Easting Zone for MGRS coordinate";

      my $MGRS           = "$zone$lett-east$lett-north$mgrs-east$mgrs-north";
     ($MGRS);
   }

   sub latlon-to-mgrs(Str $ellips, Real $latitude, Real $longitude) is export {
       my ($zone,$x-coord,$y-coord) = |latlon-to-utm($ellips, $latitude, $longitude);
       my $mgrs-string              = |utm-to-mgrs($zone,$x-coord,$y-coord);
       ($mgrs-string);
   }

   sub mgrs-to-utm(Str $mgrs-string is copy) is export {
      my $zone            = $mgrs-string.substr(0,2);
         $mgrs-string     = "0" ~ $mgrs-string if $zone !~~ /^\d+$/;
         $zone            = $mgrs-string.substr(0,3);
      my $zone-number     = $zone;
      my Str $zone-letter = $zone-number;
         $zone-number    ~~ s/^(\d+)(.*)//;
         $zone-number     = +$0;
         $zone-letter     = ~$1;

      fail "UTM zone ($zone-letter) invalid."
        unless valid-utm-zone $zone-letter;

      my $first-letter = $mgrs-string.substr(3,1);
      fail "MGRS zone ($first-letter) invalid."
        unless $first-letter ~~ /<[ABCDEFGHJKLMNPQRSTUVWXYZ]>/;

      my $second-letter = $mgrs-string.substr(4,1);
      fail "MGRS zone ($second-letter) invalid."
        unless $second-letter ~~ /<[ABCDEFGHJKLMNPQRSTUV]>/;

      my $coords    = $mgrs-string.substr(5, Inf);
      my $coord-len = $coords.chars;
      fail "MGRS coords ($coords) invalid."
        unless (0 < $coord-len <= 10) and !($coord-len % 2);

      $coord-len  = ($coord-len/2).Int;
      my $x-coord = $coords.substr(0,$coord-len);
      my $y-coord = $coords.substr($coord-len, Inf);
      $x-coord   *= 10 ** (5 - $coord-len);
      $y-coord   *= 10 ** (5 - $coord-len);

      my $east-pos
        =  ( $first-letter ~~ /<[ABCDEFGH]>/) ?? index('ABCDEFGH',$first-letter)
        !! ( $first-letter ~~ /<[JKLMNPQR]>/) ?? index('JKLMNPQR',$first-letter)
        !! ( $first-letter ~~ /<[STUVWXYZ]>/) ?? index('STUVWXYZ',$first-letter)
        !! fail "Could not calculate MGRS Easting zone.";
      fail "MGRS Letter $first-letter invalid." if $east-pos < 0;
      $east-pos++;
      $east-pos *= 100000;
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
          $tmpNorth    *= 8;
          $tmpNorth    *= 10/9;
          $tmpNorth     = ((($tmpNorth - $north-pos)/20)+0.5).Int*20;
          $north-pos   += $tmpNorth;
          $north-pos   *= 100000;
          $north-pos   -= 100000;
          $y-coord     += $north-pos;
      }
      else { #Southern Hemisphere
          my $tmpNorth = index('CDEFGHJKLM',$zone-letter);
          $tmpNorth    *= 8;
          $tmpNorth    *= 10/9;
          $tmpNorth     = ((($tmpNorth-$north-pos)/20)+0.5).Int*20;
          $north-pos   += $tmpNorth;
          $north-pos   *= 100000;
          $north-pos   -= 100000;
          $north-pos   += 2000000 if $zone-letter ne "C";
          $y-coord     += $north-pos;
      }

      ($zone,$x-coord,$y-coord);
   }

   sub mgrs-to-latlon(Str $ellips, Str $mgrs-string) is export {
      my ($zone,$x-coord,$y-coord) = |mgrs-to-utm($mgrs-string);
      my ($latitude,$longitude)    = |utm-to-latlon($ellips,$zone,$x-coord,$y-coord);
      ($latitude,$longitude);
   }

} # end module

=begin pod
=head1 NAME

Geo::Coordinates::UTM - Perl extension for Latitude Longitude conversions.

=head1 SYNOPSIS

use Geo::Coordinates::UTM;

my ($zone,$easting,$northing)=|latlon-to-utm($ellipsoid,$latitude,$longitude);

my ($latitude,$longitude)=|utm-to-latlon($ellipsoid,$zone,$easting,$northing);

my ($zone,$easting,$northing)=|mgrs-to-utm($mgrs);

my ($latitude,$longitude)=|mgrs-to-latlon($ellipsoid,$mgrs);

my ($mgrs)=|utm-to-mgrs($zone,$easting,$northing);

my ($mgrs)=|latlon-to-mgrs($ellipsoid,$latitude,$longitude);

my @ellipsoids=ellipsoid-names;

my($name, $r, $sqecc) = |ellipsoid-info 'WGS-84';

=head1 DESCRIPTION

This module will translate latitude longitude coordinates to Universal Transverse Mercator(UTM) coordinates and vice versa.

=head2 Mercator Projection

The Mercator projection was first invented to help mariners. They needed
to be able to draw a straight line on a map and follow that bearing to
arrive at a destination. In order to do this,
Mercator invented a projection which preserved angle, by projecting the
earth's surface onto a cylinder, sharing the same axis as the earth
itself. This caused all Latitude and Longitude lines to be straight and
to intersect at a 90 degree angle, but the downside was that the scale of
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
dividing the surface of the Earth into 6 degree zones, each mapped by
the Transverse Mercator projection with a central meridian in the center
of the zone. UTM zone numbers designate 6 degree longitudinal strips
extending from 80 degrees South latitude to 84 degrees North latitude.
UTM zone characters designate 8 degree zones extending north and south
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
distortion at the zone boundary, 3 degrees away from the CM is
approximately 1%.

=head2 Datums and Ellipsoids

Unlike local surveys, which treat the Earth as a plane, the precise
determination of the latitude and longitude of points over a broad area
must take into account the actual shape of the Earth. To achieve the
precision necessary for accurate location, the Earth cannot be assumed
to be a sphere. Rather, the Earth's shape more closely approximates an
ellipsoid (oblate spheroid): flattened at the poles and bulging at the
Equator. Thus the Earth's shape, when cut through its polar axis,
approximates an ellipse. A "Datum" is a standard representation of shape
and offset for coordinates, which includes an ellipsoid and an origin.
You must consider the Datum when working with geospatial data, since
data with two different Datum will not line up. The difference can be as
much as a kilometer!

=head1 EXAMPLES

A description of the available ellipsoids and sample usage of the conversion routines follows

=head2 Ellipsoids

The Ellipsoids available are as follows:

=item 1 Airy

=item 2 Australian National

=item 3 Bessel 1841

=item 4 Bessel 1841 (Nambia)

=item 5 Clarke 1866

=item 6 Clarke 1880

=item 7 Everest 1830 (India)

=item 8 Fischer 1960 (Mercury)

=item 9 Fischer 1968

=item 10 GRS 1967

=item 11 GRS 1980

=item 12 Helmert 1906

=item 13 Hough

=item 14 International

=item 15 Krassovsky

=item 16 Modified Airy

=item 17 Modified Everest

=item 18 Modified Fischer 1960

=item 19 South American 1969

=item 20 WGS 60

=item 21 WGS 66

=item 22 WGS-72

=item 23 WGS-84

=item 24 Everest 1830 (Malaysia)

=item 25 Everest 1956 (India)

=item 26 Everest 1964 (Malaysia and Singapore)

=item 27 Everest 1969 (Malaysia)

=item 28 Everest (Pakistan)

=item 29 Indonesian 1974

=item 30 Arc 1950

=item 31 NAD 27

=item 32 NAD 83

=head2 ellipsoid-names

The ellipsoids can be accessed using C<ellipsoid-names>. To store these into an array you could use 

     my @names = ellipsoid-names;

=head2 ellipsoid-info

Ellipsoids may be called either by name, or number. To return the ellipsoid information,
( "official" name, equator radius and square eccentricity)
you can use C<ellipsoid-info> and specify a name. The specified name can be numeric
(for compatibility reasons) or a more-or-less exact name.
Any text between parentheses will be ignored.

     my($name, $r, $sqecc) = |ellipsoid-info 'wgs84';
     my($name, $r, $sqecc) = |ellipsoid-info 'WGS 84';
     my($name, $r, $sqecc) = |ellipsoid-info 'WGS-84';
     my($name, $r, $sqecc) = |ellipsoid-info 'WGS-84 (new specs)';
     my($name, $r, $sqecc) = |ellipsoid-info 23;

=head2 latlon-to-utm

Latitude values in the southern hemisphere should be supplied as negative values
(e.g. 30 deg South will be -30). Similarly Longitude values West of the meridian
should also be supplied as negative values. Both latitude and longitude should
not be entered as deg,min,sec but as their decimal equivalent,
e.g. 30 deg 12 min 22.432 sec should be entered as 30.2062311

The ellipsoid value should correspond to one of the numbers above,
e.g. to use WGS-84, the ellipsoid value should be 23

For latitude  57deg 49min 59.000sec North
    longitude 02deg 47min 20.226sec West

using Clarke 1866 (Ellipsoid 5)

     ($zone,$east,$north)=|latlon-to-utm('clarke 1866',57.803055556,-2.788951667)

returns 

     $zone  = 30V
     $east  = 512543.777159849
     $north = 6406592.20049111

On occasions, it is necessary to map a pair of (latitude, longitude)
coordinates to a predefined zone. This is done by providing a value for the optional named parameter zone as follows:

     ($zone, $east, $north)=|latlon-to-utm('international', :zone($zone-number),
                                          $latitude, $longitude)

For instance, Spain territory goes over zones 29, 30 and 31 but
sometimes it is convenient to use the projection corresponding to zone
30 for all the country.

Santiago de Compostela is at 42deg 52min 57.06sec North, 8deg 32min 28.70sec West

    ($zone, $east, $norh)=|latlon-to-utm('international',  42.882517, -8.541306)

returns

     $zone = 29T
     $east = 537460.331
     $north = 4747955.991

but forcing the conversion to zone 30:

    ($zone, $east, $norh)=|latlon-to-utm('international', :zone(30),
                                         42.882517, -8.541306)

returns

    $zone = 30T
    $east = 47404.442
    $north = 4762771.704

=head2 utm-to-latlon

Reversing the above example,

     ($latitude,$longitude)=|utm-to-latlon(5,'30V',512543.777159849,6406592.20049111)

returns

     $latitude  = 57.8030555601332
     $longitude = -2.7889516669741

     which equates to

     latitude  57deg 49min 59.000sec North
     longitude 02deg 47min 20.226sec West


=head2 latlon-to-mgrs

Latitude values in the southern hemisphere should be supplied as negative values
(e.g. 30 deg South will be -30). Similarly Longitude values West of the meridian
should also be supplied as negative values. Both latitude and longitude should
not be entered as deg,min,sec but as their decimal equivalent,
e.g. 30 deg 12 min 22.432 sec should be entered as 30.2062311

The ellipsoid value should correspond to one of the numbers above, e.g. to use WGS-84, the ellipsoid value should be 23

For latitude  57deg 49min 59.000sec North
    longitude 02deg 47min 20.226sec West

using WGS84 (Ellipsoid 23)

     ($mgrs)=|latlon-to-mgrs(23,57.8030590197684,-2.788956799)

returns 

     $mgrs  = 30VWK1254306804

=head2 mgrs-to-latlon

Reversing the above example,

     ($latitude,$longitude)=|mgrs-to-latlon(23,'30VWK1254306804')

returns

     $latitude  = 57.8030590197684
     $longitude = -2.788956799645

=head2 mgrs-to-utm

    Similarly it is possible to convert MGRS directly to UTM

        ($zone,$easting,$northing)=|mgrs-to-utm('30VWK1254306804')

    returns

        $zone = 30V
        $easting = 512543
        $northing = 6406804

=head2 utm-to-mgrs

    and the inverse converting from UTM yo MGRS is done as follows

       ($mgrs)=|utm-to-mgrs('30V',512543,6406804);

    returns
        $mgrs = 30VWK1254306804

=head1 AUTHOR

Graham Crookham, grahamc@cpan.org

Kevin Pye, kjpye@cpan.org

=head1 THANKS

Thanks go to the following:

Felipe Mendonca Pimenta for helping out with the Southern hemisphere testing.

Michael Slater for discovering the Escape \Q bug.

Mark Overmeer for the ellipsoid-info routines and code review.

Lok Yan for the >72deg. N bug.

Salvador Fandino for the forced zone UTM and additional tests

Matthias Lendholt for modifications to MGRS calculations

Peder Stray for the short MGRS patch



=head1 COPYRIGHT

Copyright (c) 2000,2002,2004,2007,2010,2013 by Graham Crookham.  All rights reserved.

copyright (c) 2018 by Kevin Pye.
    
This package is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.             

=end pod
