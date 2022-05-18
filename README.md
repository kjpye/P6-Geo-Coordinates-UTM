[![Actions Status](https://github.com/kjpye/P6-Geo-Coordinates-UTM/actions/workflows/test.yml/badge.svg)](https://github.com/kjpye/P6-Geo-Coordinates-UTM/actions)

NAME
====

Geo::Coordinates::UTM - Perl extension for Latitude Longitude conversions.

SYNOPSIS
========

use Geo::Coordinates; use Geo::Coordinates::UTM;

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

DESCRIPTION
===========

This module will translate latitude longitude coordinates to Universal Transverse Mercator(UTM) coordinates and vice versa.

Mercator Projection
-------------------

The Mercator projection was first invented to help mariners. They needed to be able to draw a straight line on a map and follow that bearing to arrive at a destination. In order to do this, Mercator invented a projection which preserved angle, by projecting the earth's surface onto a cylinder, sharing the same axis as the earth itself. This caused all Latitude and Longitude lines to be straight and to intersect at a 90° angle, but the downside was that the scale of the map increased as you moved away from the equator so that the lines of longitude were parallel.

Because the scale varies, areas near the poles appear much larger on the map than a similar sized object near the equator. The Mercator Projection is useless near the poles since the scale becomes infinite.

Transverse Mercator Projection
------------------------------

A Transverse Mercator projection takes the cylinder and turns it on its side. Now the cylinder's axis passes through the equator, and it can be rotated to line up with the area of interest. Many countries use Transverse Mercator for their grid systems. The disadvantage is that now neither the lines of latitude or longitude (apart from the central meridian) are straight.

Universal Transverse Mercator
-----------------------------

The Universal Transverse Mercator(UTM) system sets up a universal world wide system for mapping. The Transverse Mercator projection is used, with the cylinder in 60 positions. This creates 60 zones around the world. Positions are measured using Eastings and Northings, measured in meters, instead of Latitude and Longitude. Eastings start at 500,000 on the centre line of each zone. In the Northern Hemisphere, Northings are zero at the equator and increase northward. In the Southern Hemisphere, Northings start at 10 million at the equator, and decrease southward. You must know which hemisphere and zone you are in to interpret your location globally. Distortion of scale, distance and area increase away from the central meridian.

UTM projection is used to define horizontal positions world-wide by dividing the surface of the Earth into 6° zones, each mapped by the Transverse Mercator projection with a central meridian in the center of the zone. UTM zone numbers designate 6° longitudinal strips extending from 80° South latitude to 84° North latitude. UTM zone characters designate 8° zones extending north and south from the equator. Eastings are measured from the central meridian (with a 500 km false easting to insure positive coordinates). Northings are measured from the equator (with a 10,000 km false northing for positions south of the equator).

UTM is applied separately to the Northern and Southern Hemisphere, thus within a single UTM zone, a single X / Y pair of values will occur in both the Northern and Southern Hemisphere. To eliminate this confusion, and to speed location of points, a UTM zone is sometimes subdivided into 20 zones of Latitude. These grids can be further subdivided into 100,000 meter grid squares with double-letter designations. This subdivision by Latitude and further division into grid squares is generally referred to as the Military Grid Reference System (MGRS). The unit of measurement of UTM is always meters and the zones are numbered from 1 to 60 eastward, beginning at the 180th meridian. The scale distortion in a north-south direction parallel to the central meridian (CM) is constant However, the scale distortion increases either direction away from the CM. To equalize the distortion of the map across the UTM zone, a scale factor of 0.9996 is applied to all distance measurements within the zone. The distortion at the zone boundary, 3°s away from the CM is approximately 1%.

Ellipsoids
----------

Ellipsoids are imported from Geo::Ellipsoids.

latlon-to-utm
-------------

`latlon-to-utm` is the main routine for converting latitude and longitude to UTΜcoordinates. It has various incantations depending on the form of the input, but the UTM coordinates are always expressed as a `PointUTM` object, or a collection of `PointUTM` objects (`LineStringUTM` or `PolygonUTM`).

A `PointUTM` has three accessor methods: `zone`, `easting`, and `northing`. In addition, and mainly for backwards compatibility with older versions of this module, a method `List` is provided so that the return value of the latlon-to-utm method can be used as a list of zone, easting and northing.

`latlon-to-utm` can be called with explicit values for latitude and longitude:

    my $utm = latlon-to-utm($ellipsoid, $latitude, $longitude);

with the position expressed as a `Geo::Geometry` point type (`Point`, `PointZ`, `PointM`, or `PointZM`):

    my $position = Point.new($longitude, %latitude_;
    my $utm = latlon-to-utm($ellipsoid, $position);

with multiple positions expressed as a `Geo::Geometry` line type (`LineString`, `LineStringZ`, `LineStringM`, or `LineStringZM`):

    my $line = LineString.new(...);
    my $utm = latlon-to-utm($ellipsoid, $line);

or with multiple positions expressed as a `Geo::Geometry` polygon type (`Polygon`, `PolygonZ`, `PolygonM`, or `PolygonZM`):

    my $polygon = Polygon.new(...);
    my $utm = latlon-to-utm($ellipsoid, $polygon);

In the latter two cases, each point within the line or polygon will be converted to a `PointUTM` object, and the returned value will be a `LineStringUTM` or `PolygonUTM` object as appropriate.

A `LineStringUTM` object provides a single accessor method `points` which returns an array of `PointUTM` objects. A `PolygonUTM` object provides a single accessor method `rings` which returns an array of `LinearRingUTM` objects representing the polygon's external boundary and any internal holes. A `LinearRingUTM` object provides a single accessor method `points` which returns an array of `PointUTM` objects.

Latitude values in the southern hemisphere should be supplied as negative values (e.g. 30° South will be -30). Similarly Longitude values West of the meridian should also be supplied as negative values. Both latitude and longitude should not be entered as deg,min,sec but as their decimal equivalent, e.g. 30°12'22.432" sec should be entered as 30.2062311

Previous versions of this module allowed the ellipsoid to be expressed as an index into the array of ellipsoids. This usage was marked as deprecated, and is now removed.

For latitude 57°49'59.000" North longitude 02°47'20.226" West

using Clarke 1866 (Ellipsoid 5)

    ($zone,$east,$north)= |latlon-to-utm('clarke 1866',57.803055556,-2.788951667)

returns 

    $zone  = "30V"
    $east  = 512543.777159849
    $north = 6406592.20049111

On occasions, it is necessary to map a pair of (latitude, longitude) coordinates to a predefined zone. This is done by providing a value for the optional named parameter zone as follows:

    ($zone, $east, $north)= |latlon-to-utm('international', :zone($zone-number),
                                         $latitude, $longitude)

For instance, Spain territory goes over zones 29, 30 and 31 but sometimes it is convenient to use the projection corresponding to zone 30 for all the country.

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

This is also necessary when the eastern boundary of a map lies along a zone boundary. If the zone is not forced for values on that boundary then the returned values will not be useful.

utm-to-latlon
-------------

Reversing the above example,

    ($latitude,$longitude)= |utm-to-latlon(5,'30V',512543.777159849,6406592.20049111)

returns

    $latitude  = 57.8030555601332
    $longitude = -2.7889516669741

    which equates to

    latitude  57°49'59.000" North
    longitude 02°47'20.226" West

To allow compatibility with previous versions of this module, `utm-to-latlon` returns an object of type `PointLatLon` which contains only the two attributes `x` (the longitude) and `y` (the latitude). If used as in the above example, the List method will be called which will return a list of the latitude and longitude. There is also a `Point` method to allow coercion to a more standard type.

latlon-to-mgrs
--------------

`latlon-to-mgrs` is called in the same way as `latlon-to-utm`, but instead of a `PointUTM` object, the returned value will be a string contain the MGRS representation of the location.

For latitude 57°49'59.000" North longitude 02°47'20.226" West

using WGS84 (Ellipsoid 23)

    $mgrs = latlon-to-mgrs('WGS-84', 57.8030590197684, -2.788956799)

returns 

    $mgrs  = 30VWK1254306804

Currently only the version explicitly specifying longitude and latitude is implemented; Other versions will follow.

Note that previous versions of this module returned a list containing a single value. This is no longer the case and might cause problems in some circumstances.

mgrs-to-latlon
--------------

Reversing the above example,

    ($latitude,$longitude)=|mgrs-to-latlon(23,'30VWK1254306804')

returns

    $latitude  = 57.8030590197684
    $longitude = -2.788956799645

Like utm-to-latlon, the returned object is `PointLatLon`.

mgrs-to-utm
-----------

    Similarly it is possible to convert MGRS directly to UTM

        ($zone,$easting,$northing)=|mgrs-to-utm('30VWK1254306804')

    returns

        $zone = 30V
        $easting = 512543
        $northing = 6406804

utm-to-mgrs
-----------

and the inverse converting from UTM to MGRS is done as follows

    ($mgrs)=|utm-to-mgrs('30V',512543,6406804);

returns $mgrs = 30VWK1254306804

AUTHOR
======

Kevin Pye, kjpye@cpan.org

Graham Crookham, grahamc@cpan.org

THANKS
======

Thanks go to the following:

Felipe Mendonca Pimenta for helping out with the Southern hemisphere testing.

Michael Slater for discovering the Escape \Q bug.

Mark Overmeer for the ellipsoid-info routines and code review.

Lok Yan for the >72deg. N bug.

Salvador Fandino for the forced zone UTM and additional tests

Matthias Lendholt for modifications to MGRS calculations

Peder Stray for the short MGRS patch

COPYRIGHT
=========

Copyright (c) 2000,2002,2004,2007,2010,2013 by Graham Crookham. All rights reserved.

copyright (c) 2018, 2022 by Kevin Pye.

This package is free software; you can redistribute it and/or modify it under the same terms as Raku itself.

