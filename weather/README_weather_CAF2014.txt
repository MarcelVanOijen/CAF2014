The first line of each CAF2014 weather file should contain the following variable names:

ST	year	doy	GR	TMIN	TMAX	VP	WN	RAIN

These nine variable names must be spelled EXACTLY as shown, because the model identifies the variables using their name.
However, the variables may appear in any order from left to right.
The units of the weather variables should be the following:
GR: MJ m-2 d-1, TMIN and TMAX: degree Celsius, VP: kPA, WN: m s-1, RAIN: mm d-1

EXAMPLE of possible file contents (only showing first and last two lines):

ST	year	doy	GR	TMIN	TMAX	VP	WN	RAIN
11	1998	1	10.1	17.3	27.2	2.10	0.59	0.0		
(...)
11	2004	364	12.8	16.0	27.0	2.11	0.67	6.3		
11	2004	365	17.5	15.4	27.3	2.13	0.81	1.8		
