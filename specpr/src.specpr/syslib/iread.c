/*  machine dependency added 3/27/85. Roger Clark */

iread(fd,cnt,str,stlen)

short *fd,*cnt; long stlen; char str[];
{
	return( read(*fd,str,*cnt) );
}


iread_(fd,cnt,str,stlen)

short *fd,*cnt; long stlen; char str[];
{
	return( read(*fd,str,*cnt) );
}
