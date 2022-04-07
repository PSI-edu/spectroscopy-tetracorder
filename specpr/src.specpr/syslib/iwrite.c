/*  machine dependency added 3/27/85. Roger Clark */

iwrite(fd,cnt,str,stlen)

short *fd,*cnt; long stlen; char str[];
{
	return( write(*fd,str,*cnt) );
}


iwrite_(fd,cnt,str,stlen)

short *fd,*cnt; long stlen; char str[];
{
	return( write(*fd,str,*cnt) );
}
