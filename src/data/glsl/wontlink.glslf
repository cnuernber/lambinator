/* Fragment shader */

varying vec2 screensize;

void main()
{
	gl_FragColor[0] = gl_FragCoord[0] / screensize.x;
	gl_FragColor[1] = gl_FragCoord[1] / screensize.y;
	gl_FragColor[2] = 1.0;
}
