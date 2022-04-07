#include <stdio.h>
#include <sys/types.h>
#include <sys/mtio.h>
#include <ctype.h>
#include "../parser/label1.h"

#define tape "/dev/nrmt0"

unsigned char tape_buffer[512];
struct specrec disk_buffer;

struct mtget tape_status;
struct mtop	 tape_operation;

int	tape_fd;
int	disk_fd;

int tape_type;
int	scan_time;
int	int_time;

main(ac,av)
int ac; char **av;
{
	int	i;
	int	read_count;
	int	record_count = 0;
	int startup = 1;
	char	input_line[256];

	if (ac!=2) {
		fprintf(stderr,"Usage: wejfmt file_name\n");
		exit(-1); 
	}
	tape_fd = open(tape, 0);
	if (tape_fd < 0) {
		fprintf(stderr,"Can't open %s ",tape);
		perror("because");
		exit(-1);
	}
	disk_fd = creat(av[1],0666);
	if (disk_fd < 0) {
		fprintf(stderr,"Can't create %s ",av[1]);
		perror("because");
		exit(-1);
	}
	i = write(disk_fd, &disk_buffer, sizeof disk_buffer);

	while (tape_type==0) {
		printf("Wedge tape transfer routine\n");
		printf("----- ---- -------- -------\n");
		printf("\n");
		printf("Type in the wedge data system version.\n");
		printf("\n");
		printf("Type:    1  old wedge system. (before Dec. 1979)\n");
		printf("            (10 sec. per rev., 16ms per half chop\n");
		printf("         2  wedge system after Dec. 1979\n");
		printf("            (20 sec. per rev., 32ms per half chop\n");
		printf("         3  wedge system after Nov. 1, 1980\n");
		printf("            (80 sec. per rev., 128ms per half chop\n");
		printf("         4  combinations 1 and 3 above,  1 when ");
		printf("\"VIS\" is found in title\n");
		printf("            default = #3\n");
		fgets(input_line, sizeof input_line, stdin);
		i = sscanf(input_line, "%d", &tape_type);
		if (tape_type<0 || tape_type>4) {
			tape_type = 0;
			continue;
		}
		if (tape_type==0) tape_type = 3;
		break;
	}

	tape_operation.mt_op = MTFSR;
	tape_operation.mt_count = 2;
	i = ioctl(tape_fd, MTIOCTOP, &tape_operation);

	while (startup) {
		read_count = read(tape_fd, tape_buffer, sizeof tape_buffer);
		switch (read_count) {
			case 0:
				break;
			case -1:
				ioctl(tape_fd, MTIOCGET, &tape_status);
				printf ("Read error: device status=%x, error register=%x\n",
					tape_status.mt_dsreg,
					tape_status.mt_erreg);
				tape_operation.mt_op = MTFSF;
				tape_operation.mt_count = 1;
				ioctl(tape_fd, MTIOCTOP, &tape_operation);
				tape_operation.mt_op = MTFSR;
				tape_operation.mt_count = 1;
				ioctl(tape_fd, MTIOCTOP, &tape_operation);
				break;
			default:
				printf("record %d '%32.32s'\n",
					++record_count,
					&tape_buffer[64]);
				startup = 0;
				while (read_count<sizeof tape_buffer)
					tape_buffer[read_count++] = '\0';
				write_record(ac,av);
				break;
		}
	}
	while (1) {
		read_count = read(tape_fd, tape_buffer, sizeof tape_buffer);
		switch (read_count) {
			case 0:
			case -1:
				ioctl(tape_fd, MTIOCGET, &tape_status);
				printf ("Read error: device status=%x  error register=%x\n",
					tape_status.mt_dsreg,
					tape_status.mt_erreg);
				break;
			default:
				printf("record %d '%32.32s'\n",
					++record_count,
					&tape_buffer[64]);
				while (read_count<sizeof tape_buffer)
					tape_buffer[read_count++] = '\0';
				write_record(ac,av);
				break;
		}
	}

	exit(0);
}


char history[61] = "   raw data                                                 ";
char man_hist[297] = "                                                                                                                                                                                                                                                                                                        ";

int output_record = 0;

write_record(ac,av)
char **av;
{
	char *dummy;
	char title[41];
	int i,j;

	dummy = (char *) &disk_buffer;
	for (i=0; i<1536; i++)
		*dummy++ = '\0';

	strncpy(disk_buffer.title, &tape_buffer[64], 32);
	strncpy(&disk_buffer.title[32], "        ", 8);
	strncpy(title, &disk_buffer, 40);
	title[40] = '\0';
	for (i=0; i<40; i++)
		if (isupper(disk_buffer.title[i])) disk_buffer.title[i] =
			tolower(disk_buffer.title[i]);
	strncpy(disk_buffer.ihist, history, 60);
	strncpy(disk_buffer.mhist, man_hist, 296);
	for (i=0; i<3; i++) {
		disk_buffer.cta[2*i] = ((tape_buffer[0140+i]>>4) & 017) + '0';
		disk_buffer.cta[2*i+1] = (tape_buffer[0140+i] & 017) +'0';
		disk_buffer.ctb[2*i] = ((tape_buffer[0144+i]>>4) & 017) + '0';
		disk_buffer.ctb[2*i+1] = (tape_buffer[0144+i] & 017) +'0';
		disk_buffer.sta[2*i] = ((tape_buffer[0150+i]>>4) & 017) + '0';
		disk_buffer.sta[2*i+1] = (tape_buffer[0150+i] & 017) +'0';
		disk_buffer.stb[2*i] = ((tape_buffer[0154+i]>>4) & 017) + '0';
		disk_buffer.stb[2*i+1] = (tape_buffer[0154+i] & 017) +'0';
		disk_buffer.datea[2*i] = ((tape_buffer[0160+i]>>4) & 017) + '0';
		disk_buffer.datea[2*i+1] = (tape_buffer[0160+i] & 017) +'0';
		disk_buffer.dateb[2*i] = ((tape_buffer[0164+i]>>4) & 017) + '0';
		disk_buffer.dateb[2*i+1] = (tape_buffer[0164+i] & 017) +'0';
	}

	disk_buffer.revs =  1000 * ((tape_buffer[0170]>>4) & 017);
	disk_buffer.revs += 100 * tape_buffer[0170] & 017;
	disk_buffer.revs += 10 * ((tape_buffer[0171]>>4) & 017);
	disk_buffer.revs += 1 * tape_buffer[0171] & 017;

	disk_buffer.filen = ++output_record;

	for (i=0; i<120; i++) {
		j = tape_buffer[0200+i*3] * 65536
		  + tape_buffer[0201+i*3] * 256
		  + tape_buffer[0202+i*3];
		disk_buffer.data[i] = (j>8388608) ? j-16777216 : j;
	}

	disk_buffer.nruns[0] = 1;
	disk_buffer.iwtrns = 1;

	switch (tape_type) {
		case 1:
			scan_time = 10;
			int_time = 16;
			break;

		case 2:
			scan_time = 20;
			int_time = 32;
			break;

		case 3:
			scan_time = 80;
			int_time = 128;
			break;

		case 4:
			scan_time = 80;
			int_time = 128;
			if ( (i=index(title), 'V') && !strncmp(i,"IS")) {
				scan_time = 10;
				int_time = 16;
			}
			break;
	}
	disk_buffer.scatim = scan_time;
	disk_buffer.timint = scan_time * disk_buffer.revs;
	disk_buffer.itimch = int_time;
	disk_buffer.xnrm = 1.0;

	i = write(disk_fd, &disk_buffer, sizeof disk_buffer);
	if (i!=sizeof disk_buffer) {
		fprintf(stderr,"Can't write on %s",av[1]);
		perror("because");
	}
}
