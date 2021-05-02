;;; "solid.scm" Solid Modeling with VRML97
; Copyright 2001 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(require 'printf)
(require 'array)
(require 'array-for-each)
(require 'color)

;;@ifset html
;;<A NAME="Solid">
;;@end ifset
;;@code{(require 'solid)}
;;@ifset html
;;</A>
;;@end ifset
;;@ftindex solids
;;@ftindex solid
;;@ftindex solid-modeling
;;
;;@noindent
;;@uref{http://swissnet.ai.mit.edu/~jaffer/Solid/#Example} gives an
;;example use of this package.

(define pi/180 (/ pi 180))

;;@body Returns the VRML97 string (including header) of the concatenation
;;of strings @1, @dots{}.
(define (vrml . nodes)
  (apply vrml-append (sprintf #f "#VRML V2.0 utf8\\n") nodes))

;;@body Returns the concatenation with interdigitated newlines of
;;strings @1, @2, @dots{}.
(define (vrml-append node1 . node2)
  (define nl (string #\newline))
  (apply string-append
	 node1
	 (apply append (map (lambda (node) (list nl node)) node2))))

;;@body Writes to file named @1 the VRML97 string (including header) of
;;the concatenation of strings @2, @dots{}.
(define (vrml-to-file file . nodes)
  (call-with-output-file file
    (lambda (oprt)
      (for-each (lambda (str) (display str oprt) (newline oprt))
		(cons (sprintf #f "#VRML V2.0 utf8") nodes)))))

;;@body Returns a VRML97 string setting the title of the file in which
;;it appears to @1.  Additional strings @2, @dots{} are comments.
(define (world:info title . info)
  (string-append
   (apply string-append
	  (sprintf #f "WorldInfo {title %#a info  [" title)
	  (map (lambda (str) (sprintf #f "  %#a\\n" str)) info))
   (sprintf #f "  ]\\n}\\n")))

;;@noindent
;;
;;VRML97 strings passed to @code{vrml} and @code{vrml-to-file} as
;;arguments will appear in the resulting VRML code.  This string turns
;;off the headlight at the viewpoint:
;;@example
;;" NavigationInfo @{headlight FALSE@}"
;;@end example

;;@body Specifies the distant images on the inside faces of the cube
;;enclosing the virtual world.
(define (scene:panorama front right back left top bottom)
  (sprintf #f "Background {%s%s%s%s%s%s}"
	   (if front  (sprintf #f "\\n  frontUrl %#a" front) "")
	   (if right  (sprintf #f "\\n  rightUrl %#a" right) "")
	   (if back   (sprintf #f "\\n  backUrl %#a" back) "")
	   (if left   (sprintf #f "\\n  leftUrl %#a" left) "")
	   (if top    (sprintf #f "\\n  topUrl %#a" top) "")
	   (if bottom (sprintf #f "\\n  bottomUrl %#a" bottom) "")))

;; 2-dimensional coordinates.
(define (coordinates2string obj)
  (if (vector? obj) (set! obj (vector->list obj)))
  (case (length obj)
    ((2) (apply sprintf #f "%g %g" obj))
    (else (slib:error 'coordinates2string obj))))

;; This one will duplicate number argument.
(define (coordinate2string obj)
  (coordinates2string (if (number? obj) (list obj obj) obj)))

;; 3-dimensional coordinates.
(define (coordinates3string obj)
  (if (vector? obj) (set! obj (vector->list obj)))
  (case (length obj)
    ((3) (apply sprintf #f "%g %g %g" obj))
    (else (slib:error 'coordinates3string obj))))

;; This one will triplicate number argument.
(define (coordinate3string obj)
  (coordinates3string (if (number? obj) (list obj obj obj) obj)))

(define (solid-color->sRGB obj)
  (cond ((not obj) #f)
	((color? obj) (map (lambda (x) (/ x 255.0)) (color->sRGB obj)))
	((list? obj) obj)
	((vector? obj) obj)
	((integer? obj)
	 (list (/ (ash obj -16) 255)
	       (/ (logand (ash obj -8) 255) 255)
	       (/ (logand obj 255) 255)))
	(else (slib:error 'solid:color? obj))))

(define (color->vrml-field obj)
  (and obj (coordinates3string (solid-color->sRGB obj))))

(define (colors->vrml-field objs)
  (if (null? objs)
      "[]"
      (sprintf #f "[ %s%s ]"
	       (color->vrml-field (car objs))
	       (apply string-append
		      (map (lambda (obj)
			     (sprintf #f ",\\n   %s" (color->vrml-field obj)))
			   (cdr objs))))))

(define (angles->vrml-field objs)
  (if (null? objs)
      "[]"
      (sprintf #f "[ %g%s ]"
	       (* pi/180 (car objs))
	       (apply string-append
		      (map (lambda (obj) (sprintf #f ", %g" (* pi/180 obj)))
			   (cdr objs))))))

(define (direction->vrml-field obj)
  (if (vector? obj) (set! obj (vector->list obj)))
  (coordinates3string
   (case (length obj)
     ((2) (let ((th (* (car obj) pi/180))
		(ph (* (cadr obj) pi/180)))
	    (list (* (sin ph) (sin th))
		  (- (cos th))
		  (* -1 (cos ph) (sin th)))))
     ((3) obj)
     (else (slib:error 'not 'direction obj)))))

;;@body
;;
;;@1 is a list of color objects.  Each may be of type @ref{Color
;;Data-Type, color}, a 24-bit sRGB integer, or a list of 3 numbers
;;between 0.0 and 1.0.
;;
;;@2 is a list of non-increasing angles the same length as
;;@1.  Each angle is between 90 and -90 degrees.  If 90 or -90 are not
;;elements of @2, then the color at the zenith and nadir are taken from
;;the colors paired with the angles nearest them.
;;
;;@0 fills horizontal bands with interpolated colors on the backgroud
;;sphere encasing the world.
(define (scene:sphere colors angles)
  (define seen0? 0)
  (if (vector? colors) (set! colors (vector->list colors)))
  (if (vector? angles) (set! angles (vector->list angles)))
  (if (not (eqv? (length colors) (length angles)))
      (slib:error 'scene:sphere 'length (length colors) (length angles)))
  ;;(@print angles)
  (cond ((< (car angles) 90)
	 (set! colors (cons (car colors) colors))
	 (set! angles (cons 90 angles))))
  (set! colors (reverse colors))
  (set! angles (reverse angles))
  (cond ((> (car angles) -90)
	 (set! colors (cons (car colors) colors))
	 (set! angles (cons -90 angles))))
  (let loop ((colors colors) (angles angles)
	     (ground-colors '()) (ground-angles '()))
    ;;(print 'loop 'angles angles 'ground-angles ground-angles)
    (cond
     ((null? angles)			; No ground colors
      (sprintf
       #f "Background {%s%s}"
       (sprintf #f "\\n  skyColor %s" (colors->vrml-field colors))
       (sprintf #f "\\n  skyAngle %s" (angles->vrml-field (cdr angles)))))
     ((and (zero? seen0?) (zero? (car angles)))
      (set! seen0? (+ 1 seen0?))
      (loop (cdr colors) (cdr angles)
	    (cons (car colors) ground-colors)
	    (cons 0 ground-angles)))
     ((>= (car angles) 0)
      (or (> seen0? 1)
	  (null? colors)
	  (null? ground-colors)
	  (zero? (car angles))
	  (let* ((sw (- (car ground-angles)))
		 (gw (car angles))
		 (avgclr
		  (map (lambda (sx gx)
			 (/ (+ (* sw sx) (* gw gx)) (+ sw gw)))
		       (solid-color->sRGB (car colors))
		       (solid-color->sRGB (car ground-colors)))))
	    (set! colors (cons avgclr colors))
	    (set! angles (cons 0 angles))
	    (set! ground-colors (cons avgclr ground-colors))
	    (set! ground-angles (cons 0 ground-angles))))
      (set! colors (reverse colors))
      (set! angles (reverse angles))
      (set! ground-colors (reverse ground-colors))
      (set! ground-angles (reverse ground-angles))
      (set! angles (map (lambda (angle) (- 90 angle)) angles))
      (set! ground-angles (map (lambda (angle) (+ 90 angle)) ground-angles))
      ;;(print 'final 'angles angles 'ground-angles ground-angles)
      (sprintf
       #f "Background {%s%s%s%s}"
       (sprintf #f "\\n  skyColor %s" (colors->vrml-field colors))
       (sprintf #f "\\n  skyAngle %s" (angles->vrml-field (cdr angles)))
       (sprintf #f "\\n  groundColor %s" (colors->vrml-field ground-colors))
       (sprintf #f "\\n  groundAngle %s" (angles->vrml-field (cdr ground-angles)))))
     (else (loop (cdr colors) (cdr angles)
		 (cons (car colors) ground-colors)
		 (cons (car angles) ground-angles))))))

;;@body Returns a blue and brown backgroud sphere encasing the world.
(define (scene:sky-and-dirt)
  (scene:sphere
   '((0.0 0.2 0.7)
     (0.0 0.5 1.0)
     (0.9 0.9 0.9)
     (0.6 0.6 0.6)
     (0.4 0.25 0.2)
     (0.2 0.1 0.0)
     (0.3 0.2 0.0))
   '(90 15 0 0 -15 -70 -90)))

;;@body Returns a blue and green backgroud sphere encasing the world.
(define (scene:sky-and-grass)
  (scene:sphere
    '((0.0 0.2 0.7)
      (0.0 0.5 1.0)
      (0.9 0.9 0.9)
      (0.6 0.6 0.6)
      (0.1 0.4 0.1)
      (0.2 0.4 0.25)
      (0.2 0.1 0.0)
      (0.3 0.2 0.0))
    '(90 15 0 0 -10 -31 -70 -90)))

(define (replicate-for-strength strength proc)
  (apply string-append
	 (vector->list (make-vector
			(inexact->exact (ceiling strength))
			(proc (/ strength (ceiling strength)))))))

;;@args latitude julian-day hour turbidity strength
;;@args latitude julian-day hour turbidity
;;
;;@1 is the virtual place's latitude in degrees.  @2 is an integer from
;;0 to 366, the day of the year.  @3 is a real number from 0 to 24 for
;;the time of day; 12 is noon.  @4 is the degree of fogginess described
;;in @xref{Daylight, turbidity}.
;;
;;@0 returns a bright yellow, distant sphere where the sun would be at
;;@3 on @2 at @1.  If @5 is positive, included is a light source of @5
;;(default 1).
(define (scene:sun latitude julian-day hour turbidity . strength)
  (require 'daylight)
  (let* ((theta_s (solar-polar (solar-declination julian-day)
			       latitude
			       (solar-hour julian-day hour)))
	 (phi_s (cadr theta_s))
	 (sun-xyz (sunlight-CIEXYZ turbidity (car theta_s)))
	 (sun-color (and sun-xyz (CIEXYZ->color sun-xyz))))
    (set! theta_s (car theta_s))
    (set! strength (if (null? strength) 1 (car strength)))
    (if (not strength) (set! strength 0))
    (vrml-append
     (if (positive? strength)
	 (light:directional sun-color (list theta_s phi_s) strength)
	 "")
     (if (positive? strength)
	 (light:ambient sun-color strength)
	 "")
     (solid:rotation
      '(0 -1 0) phi_s
      (solid:rotation
       '(1 0 0) theta_s
       (solid:translation
	'(0 150.e1 0)
	(solid:sphere .695e1 (solid:color #f #f #f #f sun-color))))))))

;;@args latitude julian-day hour turbidity strength
;;@args latitude julian-day hour turbidity
;;
;;@1 is the virtual place's latitude in degrees.  @2 is an integer from
;;0 to 366, the day of the year.  @3 is a real number from 0 to 24 for
;;the time of day; 12 is noon.  @4 is the degree of cloudiness described
;;in @xref{Daylight, turbidity}.
;;
;;@0 returns an overcast sky as it might look at @3 on @2 at @1.  If @5
;;is positive, included is an ambient light source of @5 (default 1).
(define (scene:overcast latitude julian-day hour turbidity . strength)
  (require 'daylight)
  (let* ((theta_s (solar-polar (solar-declination julian-day)
			       latitude
			       (solar-hour julian-day hour)))
	 (phi_s (cadr theta_s))
	 (sun-xyz (sunlight-CIEXYZ turbidity (car theta_s)))
	 (sun-color (and sun-xyz (CIEXYZ->color sun-xyz)))
	 (color-func (overcast-sky-color-xyY turbidity (car theta_s))))
    (set! theta_s (car theta_s))
    (set! strength (if (null? strength) 1 (car strength)))
    (if (not strength) (set! strength 0))
    (vrml-append
     (if (positive? strength)
	 (light:ambient sun-color strength)
	 "")
     (do ((elev 90 (/ elev 2))
	  (angles '() (cons elev angles))
	  (xyYs   '() (cons (color-func (- 90 elev)) xyYs)))
	 ((< elev 2)
	  (scene:sphere
	   (map (lambda (xyY) (CIEXYZ->color (xyY->XYZ xyY)))
		(reverse (xyY:normalize-colors (cons '(0 0 0) xyYs))))
	   (reverse (cons -90 angles))))))))

;;@noindent
;;Viewpoints are objects in the virtual world, and can be transformed
;;individually or with solid objects.

;;@args name distance compass pitch
;;@args name distance compass
;;Returns a viewpoint named @1 facing the origin and placed @2 from it.
;;@3 is a number from 0 to 360 giving the compass heading.  @4 is a
;;number from -90 to 90, defaulting to 0, specifying the angle from the
;;horizontal.
(define (scene:viewpoint name distance compass . pitch)
  (set! pitch (* pi/180 (if (null? pitch) 0 (car pitch))))
  (set! compass (* pi/180 compass))
  (let ((level				;fieldOfView 0.785398 (pi/4)
	 (sprintf #f "Viewpoint {position 0 0 %g description %#a}"
		  distance
		  name)))
    (define tilt
      (sprintf #f "Transform {rotation 1 0 0 %g children [%s]}\\n"
	       pitch level))
    (sprintf #f "Transform {rotation 0 -1 0 %g children [%s]}\\n"
	     compass tilt)))

;;@body Returns 6 viewpoints, one at the center of each face of a cube
;;with sides 2 * @1, centered on the origin.
(define (scene:viewpoints proximity)
  (string-append
   (scene:viewpoint "North" proximity 0)
   ;;(scene:viewpoint "Up" proximity 0 90)
   (scene:viewpoint "East" proximity 90)
   (scene:viewpoint "South" proximity 180)
   ;;(scene:viewpoint "Down" proximity 0 -90)
   (scene:viewpoint "West" proximity 270)))

;;@subheading Light Sources

;;@noindent
;;In VRML97, lights shine only on objects within the same children node
;;and descendants of that node.  Although it would have been convenient
;;to let light direction be rotated by @code{solid:rotation}, this
;;restricts a rotated light's visibility to objects rotated with it.

;;@noindent
;;To workaround this limitation, these directional light source
;;procedures accept either Cartesian or spherical coordinates for
;;direction.  A spherical coordinate is a list @code{(@var{theta}
;;@var{azimuth})}; where @var{theta} is the angle in degrees from the
;;zenith, and @var{azimuth} is the angle in degrees due west of south.

;;@noindent
;;It is sometimes useful for light sources to be brighter than @samp{1}.
;;When @var{intensity} arguments are greater than 1, these functions
;;gang multiple sources to reach the desired strength.

;;@args color intensity
;;@args color
;;Ambient light shines on all surfaces with which it is grouped.
;;
;;@1 is a an object of type @ref{Color Data-Type, color}, a 24-bit sRGB
;;integer, or a list of 3 numbers between 0.0 and 1.0.  If @1 is #f,
;;then the default color will be used.  @2 is a real non-negative number
;;defaulting to @samp{1}.
;;
;;@0 returns a light source or sources of @1 with total strength of @2
;;(or 1 if omitted).
(define (light:ambient color . intensity)
  (replicate-for-strength
   (if (null? intensity) 1 (car intensity))
   (lambda (inten)
     (sprintf #f ;;direction included for "lookat" bug.
	      "DirectionalLight {color %s ambientIntensity %g intensity 0 direction 0 1 0}\\n"
	      (or (color->vrml-field color) "1 1 1")
	      inten))))

;;@args color direction intensity
;;@args color direction
;;@args color
;;Directional light shines parallel rays with uniform intensity on all
;;objects with which it is grouped.
;;
;;@1 is a an object of type @ref{Color Data-Type, color}, a 24-bit sRGB
;;integer, or a list of 3 numbers between 0.0 and 1.0.  If @1 is #f,
;;then the default color will be used.
;;
;;@2 must be a list or vector of 2 or 3 numbers specifying the direction
;;to this light.  If @2 has 2 numbers, then these numbers are the angle
;;from zenith and the azimuth in degrees; if @2 has 3 numbers, then
;;these are taken as a Cartesian vector specifying the direction to the
;;light source.  The default direction is upwards; thus its light will
;;shine down.
;;
;;@3 is a real non-negative number defaulting to @samp{1}.
;;
;;@0 returns a light source or sources of @1 with total strength of @3,
;;shining from @2.
(define (light:directional color . args)
  (define nargs (length args))
  (let ((direction (and (>= nargs 1) (car args)))
	(intensity (and (>= nargs 2) (cadr args))))
    (replicate-for-strength
     (or intensity 1)
     (lambda (inten)
       (sprintf #f
		"DirectionalLight {color %s direction %s intensity %g}\\n"
		(or (color->vrml-field color) "1 1 1")
		(direction->vrml-field direction)
		inten)))))

;;@args attenuation radius aperture peak
;;@args attenuation radius aperture
;;@args attenuation radius
;;@args attenuation
;;
;;@1 is a list or vector of three nonnegative real numbers specifying
;;the reduction of intensity, the reduction of intensity with distance,
;;and the reduction of intensity as the square of distance.  @2 is the
;;distance beyond which the light does not shine.  @2 defaults to
;;@samp{100}.
;;
;;@3 is a real number between 0 and 180, the angle centered on the
;;light's axis through which it sheds some light.  @4 is a real number
;;between 0 and 90, the angle of greatest illumination.
(define (light:beam attenuation . args)
  (define nargs (length args))
  (list (and (>= nargs 3) (caddr args))
	(and (>= nargs 2) (cadr args))
	(coordinates3string attenuation)
	(and (>= nargs 1) (car args))))

;;@args location color intensity beam
;;@args location color intensity
;;@args location color
;;@args location
;;
;;Point light radiates from @1, intensity decreasing with distance,
;;towards all objects with which it is grouped.
;;
;;@2 is a an object of type @ref{Color Data-Type, color}, a 24-bit sRGB
;;integer, or a list of 3 numbers between 0.0 and 1.0.  If @2 is #f,
;;then the default color will be used.  @3 is a real non-negative number
;;defaulting to @samp{1}.  @4 is a structure returned by
;;@code{light:beam} or #f.
;;
;;@0 returns a light source or sources at @1 of @2 with total strength
;;@3 and @4 properties.  Note that the pointlight itself is not visible.
;;To make it so, place an object with emissive appearance at @1.
(define (light:point location . args)
  (define nargs (length args))
  (let ((color       (and (>= nargs 1) (color->vrml-field (car args))))
	(intensity   (and (>= nargs 2) (cadr args)))
	(attenuation (and (>= nargs 3) (cadr (caddr args))))
	(radius      (and (>= nargs 3) (caddr (caddr args)))))
    (replicate-for-strength
     (or intensity 1)
     (lambda (inten)
       (sprintf #f
		"PointLight {location %s color %s intensity %g attenuation %s radius %g}\\n"
		(coordinates3string location)
		color intensity attenuation radius)))))

;;@args location direction color intensity beam
;;@args location direction color intensity
;;@args location direction color
;;@args location direction
;;@args location
;;
;;Spot light radiates from @1 towards @2, intensity decreasing with
;;distance, illuminating objects with which it is grouped.
;;
;;@2 must be a list or vector of 2 or 3 numbers specifying the direction
;;to this light.  If @2 has 2 numbers, then these numbers are the angle
;;from zenith and the azimuth in degrees; if @2 has 3 numbers, then
;;these are taken as a Cartesian vector specifying the direction to the
;;light source.  The default direction is upwards; thus its light will
;;shine down.
;;
;;@3 is a an object of type @ref{Color Data-Type, color}, a 24-bit sRGB
;;integer, or a list of 3 numbers between 0.0 and 1.0.  If @3 is #f,
;;then the default color will be used.
;;
;;@4 is a real non-negative number defaulting to @samp{1}.
;;
;;@0 returns a light source or sources at @1 of @2 with total strength
;;@3.  Note that the spotlight itself is not visible.  To make it so,
;;place an object with emissive appearance at @1.
(define (light:spot location . args)
  (define nargs (length args))
  (let ((direction   (and (>= nargs 1) (coordinates3string (car args))))
	(color       (and (>= nargs 2) (color->vrml-field (cadr args))))
	(intensity   (and (>= nargs 3) (caddr args)))
	(beamwidth   (and (>= nargs 4) (car (cadddr args))))
	(cutoffangle (and (>= nargs 4) (cadr (cadddr args))))
	(attenuation (and (>= nargs 4) (caddr (cadddr args))))
	(radius      (and (>= nargs 4) (cadddr (cadddr args)))))
    (replicate-for-strength
     (or intensity 1)
     (lambda (inten)
       (sprintf #f
		"SpotLight {\\n
    location %s direction %s beamWidth %g cutOffAngle %g\\n
    color %s intensity %s attenuation %s radius %g}\\n"
		(coordinates3string location)
		direction
		color
		intensity
		(and beamwidth (* pi/180 beamwidth))
		(and cutoffangle (* pi/180 cutoffangle))
		attenuation
		radius)))))

;;@subheading Object Primitives

(define (solid:node . nodes)
  (sprintf #f "%s { %s }" (car nodes) (apply string-append (cdr nodes))))

;;@args geometry appearance
;;@args geometry
;;@1 must be a number or a list or vector of three numbers.  If @1 is a
;;number, the @0 returns a cube with sides of length @1 centered on the
;;origin.  Otherwise, @0 returns a rectangular box with dimensions @1
;;centered on the origin.  @2 determines the surface properties of the
;;returned object.
(define (solid:box geometry . appearance)
  (define geom
    (cond ((number? geometry) (list geometry geometry geometry))
	  ((vector? geometry) (vector->list geometry))
	  (else geometry)))
  (solid:node "Shape"
	      (if (null? appearance) "" (string-append (car appearance) " "))
	      "geometry "
	      (solid:node "Box" (sprintf #f "size %s"
					 (coordinate3string geom)))))

;;@args radius height appearance
;;@args radius height
;;Returns a right cylinder with dimensions @1 and @code{(abs @2)}
;;centered on the origin.  If @2 is positive, then the cylinder ends
;;will be capped.  @3 determines the surface properties of the returned
;;object.
(define (solid:cylinder radius height . appearance)
  (solid:node "Shape"
	      (if (null? appearance) "" (string-append (car appearance) " "))
	      "geometry "
	      (solid:node "Cylinder"
			  (sprintf #f "height %g radius %g%s"
				   (abs height) radius
				   (if (negative? height)
				       " bottom FALSE top FALSE"
				       "")))))

;;@args radius thickness appearance
;;@args radius thickness
;;@2 must be a positive real number.  @0 returns a circular disk
;;with dimensions @1 and @2 centered on the origin.  @3 determines the
;;surface properties of the returned object.
(define (solid:disk radius thickness . appearance)
  (solid:node "Shape"
	      (if (null? appearance) "" (string-append (car appearance) " "))
	      "geometry "
	      (solid:node "Cylinder" (sprintf #f "height %g radius %g"
					      thickness radius))))

;;@args radius height appearance
;;@args radius height
;;Returns an isosceles cone with dimensions @1 and @2 centered on
;;the origin.  @3 determines the surface properties of the returned
;;object.
(define (solid:cone radius height . appearance)
  (solid:node "Shape"
	      (if (null? appearance) "" (string-append (car appearance) " "))
	      "geometry "
	      (solid:node "Cone" (sprintf #f "height %g bottomRadius %g"
					  height radius))))

;;@args side height appearance
;;@args side height
;;Returns an isosceles pyramid with dimensions @1 and @2 centered on
;;the origin.  @3 determines the surface properties of the returned
;;object.
(define (solid:pyramid side height . appearance)
  (define si (/ side 2))
  (define hi (/ height 2))
  (solid:node "Shape"
	      (if (null? appearance) "" (string-append (car appearance) " "))
	      "geometry "
	      (solid:node "Extrusion"
			  (sprintf
			   #f "spine [0 -%g 0, 0 %g 0] scale [%g %g, 0 0]"
			   hi hi si si))))

;;@args radius appearance
;;@args radius
;;Returns a sphere of radius @1 centered on the origin.  @2 determines
;;the surface properties of the returned object.
(define (solid:sphere radius . appearance)
  (solid:node "Shape"
	      (if (null? appearance) "" (string-append (car appearance) " "))
	      "geometry "
	      (solid:node "Sphere" (sprintf #f "radius %g" radius))))

;;@args geometry appearance
;;@args geometry
;;@1 must be a number or a list or vector of three numbers.  If @1 is a
;;number, the @0 returns a sphere of diameter @1 centered on the origin.
;;Otherwise, @0 returns an ellipsoid with diameters @1 centered on the
;;origin.  @2 determines the surface properties of the returned object.
(define (solid:ellipsoid geometry . appearance)
  (cond ((number? geometry) (apply solid:sphere (* 2 geometry) appearance))
	((or (list? geometry) (vector? geometry))
	 (solid:scale
	  geometry
	  (apply solid:sphere .5 appearance)))
	(else (slib:error 'solid:ellipsoid '? (cons geometry appearance)))))

;;@subheading Surface Attributes

;;@args diffuseColor ambientIntensity specularColor shininess emissiveColor transparency
;;@args diffuseColor ambientIntensity specularColor shininess emissiveColor
;;@args diffuseColor ambientIntensity specularColor shininess
;;@args diffuseColor ambientIntensity specularColor
;;@args diffuseColor ambientIntensity
;;@args diffuseColor
;;
;;Returns an @dfn{appearance}, the optical properties of the objects
;;with which it is associated.  @2, @4, and @6 must be numbers between 0
;;and 1.  @1, @3, and @5 are objects of type @ref{Color Data-Type, color},
;;24-bit sRGB integers or lists of 3 numbers between 0.0 and 1.0.
;;If a color argument is omitted or #f, then the default color will be used.
(define (solid:color dc . args)
  (define nargs (length args))
  (set! dc (color->vrml-field dc))
  (let ((ai (and (>= nargs 1) (car args)))
	(sc (and (>= nargs 2) (color->vrml-field (cadr args))))
	(si (and (>= nargs 3) (caddr args)))
	(ec (and (>= nargs 4) (color->vrml-field (cadddr args))))
	(tp (and (>= nargs 5) (list-ref args 4))))
    (sprintf
     #f "appearance Appearance {\\n  material Material {\\n%s%s%s%s%s%s}}"
     (if dc (sprintf #f "    diffuseColor %s\\n"     dc) "")
     (if ai (sprintf #f "    ambientIntensity %g\\n" ai) "")
     (if sc (sprintf #f "    specularColor %s\\n"    sc) "")
     (if si (sprintf #f "    shininess %g\\n"        si) "")
     (if ec (sprintf #f "    emissiveColor %s\\n"    ec) "")
     (if tp (sprintf #f "    transparency %g\\n"     ai) ""))))

;;@args image color scale rotation center translation
;;@args image color scale rotation center
;;@args image color scale rotation
;;@args image color scale
;;@args image color
;;@args image
;;
;;Returns an @dfn{appearance}, the optical properties of the objects
;;with which it is associated.  @1 is a string naming a JPEG or PNG
;;image resource.  @2 is #f, a color, or the string returned by
;;@code{solid:color}.  The rest of the optional arguments specify
;;2-dimensional transforms applying to the @1.
;;
;;@3 must be #f, a number, or list or vector of 2 numbers specifying the
;;scale to apply to @1.  @4 must be #f or the number of degrees to
;;rotate @1.  @5 must be #f or a list or vector of 2 numbers specifying
;;the center of @1 relative to the @1 dimensions.  @6 must be #f or a
;;list or vector of 2 numbers specifying the translation to apply to @1.
(define (solid:texture image . args)
  (define nargs (length args))
  (let ((color       (and (>= nargs 1) (car args)))
	(scale       (and (>= nargs 2) (cadr args)))
	(rotation    (and (>= nargs 3) (caddr args)))
	(center      (and (>= nargs 4) (cadddr args)))
	(translation (and (>= nargs 5) (list-ref args 5))))
    (cond ((not color))
	  ((not (string? color))
	   (set! color (solid:color color))))
    (cond ((not color))
	  ((< (string-length color) 24))
	  ((equal? "appearance Appearance {" (substring color 0 23))
	   (set! color (substring color 23 (+ -1 (string-length color))))))
    (sprintf
     #f "appearance Appearance {%s\\n  texture ImageTexture { url %#a }%s}\\n"
     (or color "")
     image
     (if (< nargs 2)
	 ""
	 (sprintf
	  #f
	  "\\n    textureTransform TextureTransform {%s%s%s%s\\n   }\\n"
	  (if (not scale)
	      ""
	      (sprintf #f "\\n      scale %s" (coordinate2string scale)))
	  (if rotation (sprintf #f "\\n      rotation %g"
				(* pi/180 rotation))
	      "")
	  (if center
	      (sprintf #f "\\n      center %s"
		       (coordinates2string center))
	      "")
	  (if translation
	      (sprintf #f "\\n      translation %s"
		       (coordinates2string translation))
	      ""))))))

;;@subheading Aggregating Objects

;;@body Returns a row of @1 @2 objects spaced evenly @3 apart.
(define (solid:center-row-of number solid spacing)
  (define (scale-by lst scaler) (map (lambda (x) (* x scaler)) lst))
  (if (vector? spacing) (set! spacing (vector->list spacing)))
  (do ((idx (quotient (+ 1 number) 2) (+ -1 idx))
       (center (if (odd? number)
		   '(0 0 0)
		   (scale-by spacing .5))
	       (map + spacing center))
       (vrml (if (odd? number)
		 (sprintf #f "%s\\n" solid)
		 "")
	     (string-append (solid:translation (map - center) solid)
			    vrml
			    (solid:translation center solid))))
      ((not (positive? idx)) vrml)))

;;@body Returns @2 rows, @5 apart, of @1 @3 objects @4 apart.
(define (solid:center-array-of number-a number-b solid spacing-a spacing-b)
  (define (scale-by lst scaler) (map (lambda (x) (* x scaler)) lst))
  (define row (solid:center-row-of number-b solid spacing-b))
  (if (vector? spacing-a) (set! spacing-a (vector->list spacing-a)))
  (do ((idx (quotient (+ 1 number-a) 2) (+ -1 idx))
       (center (if (odd? number-a)
		   '(0 0 0)
		   (scale-by spacing-a .5))
	       (map + spacing-a center))
       (vrml (if (odd? number-b)
		 (sprintf #f "%s\\n" row)
		 "")
	     (string-append (solid:translation (map - center) row)
			    vrml
			    (solid:translation center row))))
      ((not (positive? idx)) vrml)))

;;@body Returns @3 planes, @7 apart, of @2 rows, @6 apart, of @1 @4 objects @5 apart.
(define (solid:center-pile-of number-a number-b number-c solid spacing-a spacing-b spacing-c)
  (define (scale-by lst scaler) (map (lambda (x) (* x scaler)) lst))
  (define plane (solid:center-array-of number-b number-c solid spacing-b spacing-c))
  (if (vector? spacing-a) (set! spacing-a (vector->list spacing-a)))
  (do ((idx (quotient (+ 1 number-a) 2) (+ -1 idx))
       (center (if (odd? number-a)
		   '(0 0 0)
		   (scale-by spacing-a .5))
	       (map + spacing-a center))
       (vrml (if (odd? number-b)
		 (sprintf #f "%s\\n" plane)
		 "")
	     (string-append (solid:translation (map - center) plane)
			    vrml
			    (solid:translation center plane))))
      ((not (positive? idx)) vrml)))

;;@args center
;;@1 must be a list or vector of three numbers.  Returns an upward
;;pointing metallic arrow centered at @1.
;;
;;@args
;;Returns an upward pointing metallic arrow centered at the origin.
(define (solid:arrow . location)
  (solid:translation
   (if (null? location) '#(0 0 0) (car location))
   (solid:translation
    '#(0 .17 0)
    (solid:cone .04 .06 (solid:color '#(1 0 0) .2 '#(1 1 1) .8)))
   (solid:cylinder .006 .32 (solid:color #f #f '#(1 .5 .5) .8))
   (solid:sphere .014 (solid:color '#(0 0 1) #f '#(1 1 1) 1))))

;;@subheading Spatial Transformations

;;@body @1 must be a list or vector of three numbers.  @0 Returns an
;;aggregate of @2, @dots{} with their origin moved to @1.
(define (solid:translation center . solids)
  (string-append
   (sprintf #f "Transform {translation %s children [\\n"
	    (coordinates3string center))
   (apply string-append solids)
   (sprintf #f "  ]\\n}\\n")))

;;@body @1 must be a number or a list or vector of three numbers.  @0
;;Returns an aggregate of @2, @dots{} scaled per @1.
(define (solid:scale scale . solids)
  (define scales
    (cond ((number? scale) (list scale scale scale))
	  (else scale)))
  (string-append
   (sprintf #f "Transform {scale %s children [\\n" (coordinate3string scales))
   (apply string-append solids)
   (sprintf #f "  ]\\n}\\n")))

;;@body @1 must be a list or vector of three numbers.  @0 Returns an
;;aggregate of @3, @dots{} rotated @2 degrees around the axis @1.
(define (solid:rotation axis angle . solids)
  (if (vector? axis) (set! axis (vector->list axis)))
  (set! angle (* pi/180 angle))
  (string-append
   (sprintf #f "Transform {rotation %s %g children [\\n"
	    (coordinates3string axis) angle solids)
   (apply string-append solids)
   (sprintf #f "  ]\\n}\\n")))
