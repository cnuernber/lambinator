//Just pass through the vertexes unchanged from their
//original form

void main()
{
	gl_TexCoord[0] = gl_MultiTexCoord0;
	gl_Position = ftransform();
}