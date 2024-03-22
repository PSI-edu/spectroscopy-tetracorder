import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;

public class radtran_j20_10c_e_uvvims5ctholin {

	public static void main(String[] arguments) {

		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {
			// wake up early
		}

		jradSliderFrame frame = new jradSliderFrame();
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setVisible(true);

	}
}


class jradSliderFrame extends JFrame implements ChangeListener, ActionListener {

        boolean readState = false;  //use for stand-alone compiles only
        //boolean readState = true;

	//int numSliders;
	Values[] values;
	PlotScaleValues plotScaleValues;
	Strout strout;
	String cwdJChooser = ".";

	boolean fromtextbox = false;
	double scale = 0.00001;

	JButton exitbutton;
	JButton savebutton;
        JButton readbutton;
        JButton spacebutton;
        String spacebuttonTitle = null;  //title of filename read in
        JButton spacebutton2;
        String spacebuttonTitle2= null;
        JButton scaleTitleButton;
        String scalebuttonTitle = "Plot Scale:";

	public jradSliderFrame() {

                values = InitializeValues();

        //  Setup GUI interface
                Dimension scrnSize = Utility.getScreenSize();
                System.out.println("\\# screen dimensions = " + scrnSize.width + " x " + scrnSize.height);

            //set jFrame size variables to current jFrame size relative to screen dimensions
            final int DEFAULT_WIDTH;
            final int DEFAULT_HEIGHT;
            int tmpSrcnWidth = 615;
            int tmpSrcnHeight = 810;

            if (scrnSize.width > tmpSrcnWidth) {
                DEFAULT_WIDTH = tmpSrcnWidth;
            } else {
                DEFAULT_WIDTH = scrnSize.width;
                //DEFAULT_WIDTH = scrnSize.width / 3;
            }

            if (scrnSize.height > tmpSrcnHeight) {
                DEFAULT_HEIGHT = tmpSrcnHeight;
            } else {
                DEFAULT_HEIGHT = (scrnSize.height -50);
            }

/*  //testing
            //JFrame size
                //final int DEFAULT_WIDTH = 1050;
                //final int DEFAULT_WIDTH = 600;  //great on 1920 x 1200 screen
                final int DEFAULT_WIDTH = scrnSize.width / 3;
                final int DEFAULT_HEIGHT = scrnSize.height / 2;
                //final int DEFAULT_HEIGHT = 700;
*/
		setTitle("Radtran java 10-component gui");
		setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);

                JPanel opticalPanel = new JPanel(new GridBagLayout());
                GridBagConstraints c = new GridBagConstraints();
//                if (shouldFill) {
//                    //natural height, maximum width
                    //c.fill = GridBagConstraints.BOTH;
                    c.fill = GridBagConstraints.HORIZONTAL;
//                }

		//JPanel opticalPanel = new JPanel(new GridLayout(0,5));
                JPanel commandPanel = new JPanel(new GridLayout(0,5));

                JScrollPane scrollPane = new JScrollPane(opticalPanel);
                scrollPane.setPreferredSize(new Dimension(DEFAULT_WIDTH, DEFAULT_HEIGHT));
                //scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
                scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
                scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);

            //   Create and initialize sliders within GUI
                String tmpStr = null;

            //  Add array sets of JLabel, JTextFields, and JSlider to Panel
		for (int i = 0; i < values.length; i++) {
                        c.insets = new Insets(0,0,0,0);

                        //find last line entry of mixture set, increase vertical padding at cells bottom
                        if (values[i].materialType.equals("abundance")) {
                            c.insets = new Insets(0,0,10,0);
                        }
                        //if(shouldWeightX) {
                            //c.weightx = 0.1;
                        //}
                        c.fill = GridBagConstraints.HORIZONTAL;
                        c.weightx = 0.1;
                        c.gridx = 0;
                        c.gridy = i;
			opticalPanel.add(values[i].jlable, c);

			values[i].jtextfieldCurrent.addActionListener(this);
                        c.fill = GridBagConstraints.HORIZONTAL;
                        c.weightx = 0.1;
                        c.gridx = 1;
                        c.gridy = i;
			opticalPanel.add(values[i].jtextfieldCurrent, c);

			values[i].jtextfieldStart.addActionListener(this);
                        c.fill = GridBagConstraints.HORIZONTAL;
                        c.weightx = 0.1;
                        c.gridx = 2;
                        c.gridy = i;
			opticalPanel.add(values[i].jtextfieldStart, c);

			values[i].jslider.addChangeListener(this);
                        c.fill = GridBagConstraints.HORIZONTAL;
                        c.weightx = 0.6;
                        c.gridx = 3;
                        c.gridwidth = 3;
                        c.gridy = i;
			opticalPanel.add(values[i].jslider, c);

			values[i].jtextfieldEnd.addActionListener(this);
                        c.fill = GridBagConstraints.HORIZONTAL;
                        c.weightx = 0.1;
                        c.gridx = 6;
                        c.gridwidth = 1;
                        //c.gridx = 4;
                        c.gridy = i;
			opticalPanel.add(values[i].jtextfieldEnd, c);
		}

            //  Create and add bottom command buttons and textfields (commandPanel)

            //  End Program exitbutton

		exitbutton = new JButton("Exit");
		exitbutton.addActionListener(this);
		commandPanel.add(exitbutton);

            //  Save Model Program Button

		savebutton = new JButton("Save Model");
		savebutton.addActionListener(this);
		commandPanel.add(savebutton);

            //  Read Model Program Button

		readbutton = new JButton("Read Model");
		readbutton.addActionListener(this);
		commandPanel.add(readbutton);
                //boolean readState = false;  //use for stand-alone compiles only
                //boolean readState = true;
              readbutton.setEnabled(readState);

            //  Space JPanel formatting button

                //spacer for button width
                spacebutton = new JButton(spacebuttonTitle);
                commandPanel.add(spacebutton);

                //spacer for button width
                spacebutton2 = new JButton(spacebuttonTitle2);
                commandPanel.add(spacebutton2);

            //  Add: Scale plot value textfiles
                // first set default plot scale axis values
		// startup axis values are found in the Class PlotScaleValues() module below
		plotScaleValues = new PlotScaleValues();
		plotScaleValues.PlotScaleValues();

            //add textfields to display panel
                scaleTitleButton = new JButton(scalebuttonTitle);
                commandPanel.add(scaleTitleButton);

		plotScaleValues.jtextfieldWminplot.addActionListener(this);
		commandPanel.add(plotScaleValues.jtextfieldWminplot);

		plotScaleValues.jtextfieldWmaxplot.addActionListener(this);
		commandPanel.add(plotScaleValues.jtextfieldWmaxplot);

		plotScaleValues.jtextfieldPlotmin.addActionListener(this);
		commandPanel.add(plotScaleValues.jtextfieldPlotmin);

		plotScaleValues.jtextfieldPlotmax.addActionListener(this);
		commandPanel.add(plotScaleValues.jtextfieldPlotmax);

            //  Add scrollpane and finish JFrame creation
                add(scrollPane, BorderLayout.CENTER);
                add(commandPanel, BorderLayout.SOUTH);
                pack();
                setVisible(true);

            //end of GUI Creation

            // normalize abundances using the first material found
		for (int m = 0; m < values.length; m++) {
			if(values[m].materialType.equals("abundance")) {
				fromtextbox = true;//  normalizeAbundances reactivates stateChanged()
				normalizeAbundances(m);
				fromtextbox = false;

				break;
			}
		}
	}


        @Override
	public void stateChanged(ChangeEvent evt) {
		Object source = evt.getSource();

		// prevent JTextFields setValue() from triggering stateChanged slider setText & setValue() actions
				//(endless loop)
		if (!fromtextbox) {

			for (int i = 0; i < values.length; i++) {
				if (source == values[i].jslider) {

				// slider goes from 0 to 1000
				// scaled with real numbers from values[i].start to values[i].end

					values[i].current = values[i].start + ((double)values[i].jslider.getValue() /1000.0) * (values[i].end - values[i].start);
					values[i].jtextfieldCurrent.setText(String.format("%8.7f", values[i].current));

					// normalize abundances
					if (values[i].materialType.equals("abundance")) {

						fromtextbox = true;//  normalizeAbundances reactivates stateChanged() (JSlider setValue)
						normalizeAbundances(i);
						fromtextbox = false;

					}
					//System.out.println("at printall() stateChanged method");
					printall();

				}
			}
		}

	}

        @Override
	public void actionPerformed(ActionEvent actevt) {
		Object actsource = actevt.getSource();

		fromtextbox = true;  // reset jslider setValue() from JTextFields actions
			// (disable setText() and SetValue() within stateChanged() following setValue() here)

	// test for Program Exit
		if (actsource == exitbutton) {
			//System.out.println("EXIT");
			System.out.println("x");
			System.out.println("e");
			System.out.println("x");
			System.out.println("EX");
			System.exit(0);

		}

	// test for Program Save Model
		if (actsource == savebutton) {
			//open & write save model to text file

			strout = new Strout(values);  //update the output strings
								// with current "values"

			JFileChooser fileChooser = new JFileChooser(cwdJChooser);
			int returnVal = fileChooser.showSaveDialog(null);
			String jradtranSaveModelStr = "";
			File filepath;
			File[] fileList = null;

			//select image path and filename
			if(returnVal == JFileChooser.APPROVE_OPTION) {
				jradtranSaveModelStr = fileChooser.getSelectedFile().getPath();  //get path+filename
				filepath = new File(fileChooser.getSelectedFile().getParent());  //get path
				fileList = filepath.listFiles();

				//update file chooser path to last directory accessed
				cwdJChooser = fileChooser.getSelectedFile().getParent();

				try {
					int fileListIndex = -1;

                                        // search for file (don't overwrite existing file)
					//find index for selected file and set indexIndex; else fileListIndex  remains at -1
					for (int ii=0; ii < fileList.length; ii++) {
						if (jradtranSaveModelStr.equals(fileList[ii].getPath()) ){
							fileListIndex  = ii;
							//System.out.println("file exists, index # = " + fileListIndex);
							JOptionPane.showMessageDialog(null, "File Exists, try again");
						}
					}

					if (fileListIndex == -1) {
						File savemodelFile = new File(jradtranSaveModelStr);
						FileWriter fw = new FileWriter(savemodelFile);
						for (int ip = 0; ip < strout.cntrlstr.length; ip++) {
							fw.write(strout.cntrlstr[ip] + "\n");
						}
					fw.close();
                                        JOptionPane.showMessageDialog(null, "File Saved");
					}

				} catch (IOException iox) {
					//do stuff with exception
					iox.printStackTrace();
				}
			} else {
				JOptionPane.showMessageDialog(null, "File Not Selected - returning to Main Menu");
			}
		}
/*
        //*** comment out following Read Model block for standalone compiles ***
                // test for Program Read Model
		if ((actsource == readbutton) && (readState == true)) {
			//open & read model from text file (file is restart also)
                        //test only

                        Io io = new Io();
                        String[] strArr = null;
                        String fn = "test.txt";

                        RadModel rm = io.readRadModel(fn);
                        //spacebuttonTitle = ".                                                                    .";
                        strArr = rm.getOpsStartListArray();
                        for (int i = 0; i < strArr.length; i++) {
                            System.out.println(strArr[i]);
                        }
                        //put mixture section output here

                        strArr = rm.getOpsEndListArray();
                        for (int i = 0; i < strArr.length; i++) {
                            System.out.println(strArr[i]);
                        }

                    // insert filename into spacebutton
                        spacebuttonTitle = fn;  //truncate long filenames later
                        spacebutton.setText(spacebuttonTitle);
                }
        //*** END - comment out following Read Model block for standalone compiles ***
*/

        //test for plot axes text-field inputs
                if (actsource == plotScaleValues.jtextfieldWminplot) {
                    double plotvalue = Double.valueOf(plotScaleValues.jtextfieldWminplot.getText());
                    if (plotvalue < plotScaleValues.wmaxplot) {
                        plotScaleValues.wminplot = plotvalue;
                        printall();
                    }
                    plotScaleValues.jtextfieldWminplot.setText( String.format("%8.7f", plotScaleValues.wminplot) );
                }
                if (actsource == plotScaleValues.jtextfieldWmaxplot) {
                    double plotvalue = Double.valueOf(plotScaleValues.jtextfieldWmaxplot.getText());
                    if (plotvalue > plotScaleValues.wminplot) {
                        plotScaleValues.wmaxplot = plotvalue;
                        printall();
                    }
                    plotScaleValues.jtextfieldWmaxplot.setText( String.format("%8.7f", plotScaleValues.wmaxplot) );
                }
                if (actsource == plotScaleValues.jtextfieldPlotmin) {
                    double plotvalue = Double.valueOf(plotScaleValues.jtextfieldPlotmin.getText());
                    if (plotvalue < plotScaleValues.plotmax) {
                        plotScaleValues.plotmin = plotvalue;
                        printall();
                    }
                    plotScaleValues.jtextfieldPlotmin.setText( String.format("%8.7f", plotScaleValues.plotmin) );
                }
                if (actsource == plotScaleValues.jtextfieldPlotmax) {
                    double plotvalue = Double.valueOf(plotScaleValues.jtextfieldPlotmax.getText());
                    if (plotvalue > plotScaleValues.plotmin) {
                        plotScaleValues.plotmax = plotvalue;
                        printall();
                    }
                    plotScaleValues.jtextfieldPlotmax.setText( String.format("%8.7f", plotScaleValues.plotmax) );
                }

	// test for start text-field (Present VALUE) input
		for (int i = 0; i < values.length; i++) {
			if (actsource == values[i].jtextfieldStart) {

				values[i].start = Double.valueOf(values[i].jtextfieldStart.getText());

				if(values[i].start < 0.0) values[i].start = 0.0;
				if(values[i].start > values[i].end) values[i].start = values[i].end;
				if(values[i].start > values[i].current) {
					values[i].current = values[i].start;
					values[i].jtextfieldCurrent.setText( String.format("%8.7f", values[i].current) );

					if (values[i].materialType.equals("abundance")) {
						normalizeAbundances(i);
					}

					//System.out.println("at printall() actionPerformed method - start function");
					printall();
				}
				values[i].jtextfieldStart.setText( String.format("%8.7f", values[i].start) );
				values[i].jslider.setValue((int)( 1000.0D * ((values[i].current - values[i].start) / (values[i].end - values[i].start)) ));
			}
		}

	// test for end text-field (Present VALUE) input
		for (int i = 0; i < values.length; i++) {
			if (actsource == values[i].jtextfieldEnd) {

				values[i].end = Double.valueOf(values[i].jtextfieldEnd.getText());

				if( (values[i].end > 1.0) && (values[i].materialType.equals("abundance")) ) values[i].end = 1.0;
				if( (values[i].end > 1.0) && (values[i].materialType.equals("mixture")) ) values[i].end = 1.0;

				if(values[i].end < values[i].start) values[i].end = values[i].start;
				if(values[i].end < values[i].current) {
					values[i].current = values[i].end;
					values[i].jtextfieldCurrent.setText( String.format("%8.7f", values[i].current) );

					if (values[i].materialType.equals("abundance")) {
						normalizeAbundances(i);
					}

					//System.out.println("at printall() actionPerformed method - end function");
					printall();
				}
				values[i].jtextfieldEnd.setText( String.format("%8.7f", values[i].end) );
				values[i].jslider.setValue((int)( 1000.0D * ((values[i].current - values[i].start) / (values[i].end - values[i].start)) ));
			}
		}

	// test for current text-field (Present VALUE) input
		for (int i = 0; i < values.length; i++) {
			if (actsource == values[i].jtextfieldCurrent) {

				values[i].current = Double.valueOf(values[i].jtextfieldCurrent.getText());

				if(values[i].current < values[i].start) values[i].current = values[i].start;
				if(values[i].current > values[i].end) values[i].current = values[i].end;

				values[i].jtextfieldCurrent.setText( String.format("%8.7f", values[i].current) );
				values[i].jslider.setValue((int)( 1000.0D * ((values[i].current - values[i].start) / (values[i].end - values[i].start)) ));

				// normalize abundances
				if (values[i].materialType.equals("abundance")) {
					normalizeAbundances(i);
				}

				//System.out.println("at printall() actionPerformed method - current function");
				printall();

			}
		}
		fromtextbox = false;  // done with jslider setValue() from JTextFields actions

	}

	public void printall() {
		//  print values to output (to feed into Radtran)

			strout = new Strout(values);  //update the output strings
								// with current "values"

		for (int ip = 0; ip < strout.cntrlstr.length; ip++) {
			System.out.println(strout.cntrlstr[ip]);
		}

		try {
			Thread.sleep(85);
		} catch (InterruptedException e) {
			// wake up early
		}

	}


	public void normalizeAbundances(int in) {
		int i = in;

		//if (abundBoolean) {
			double abundTotal = 0;
			double abundScale = 0;
			for (int j = 0; j < values.length ; j++) {
				if(values[j].materialType.equals("abundance")) {
					abundTotal = abundTotal + values[j].current;
				}
			}

			// sum to 1.0 (100%;  CHECKED for DIVIDE by ZERO)

			if(abundTotal == values[i].current) {
				abundScale = 0;
				values[i].current = 1.0;
				values[i].jtextfieldCurrent.setText(String.format("%8.7f", values[i].current));
				values[i].jslider.setValue((int)( 1000.0D * ((values[i].current - values[i].start) / (values[i].end - values[i].start)) ));
				values[i].end = values[i].current;
				values[i].jtextfieldEnd.setText(String.format("%8.7f", values[i].end));
			} else {
				abundScale = (1.0 - values[i].current) / (abundTotal - values[i].current);
			}
			// System.out.println("Total & Scale = " + abundTotal + " " + abundScale);

			for (int j = 0; j < values.length; j++) {
				if ( (i != j) && (values[j].materialType.equals("abundance")) ) {
					values[j].current = values[j].current * abundScale;

					if(values[j].current < 0.0) values[j].current = 0.0;
					if(values[j].current > 1.0) values[j].current = 1.0;

					if(values[j].current < values[j].start) {
						values[j].start = values[j].current;
						values[j].jtextfieldStart.setText(String.format("%8.7f", values[j].start));
					}
					if(values[j].current > values[j].end) {
						values[j].end = values[j].current;
						values[j].jtextfieldEnd.setText(String.format("%8.7f", values[j].end));
					}

					values[j].jtextfieldCurrent.setText(String.format("%8.7f", values[j].current));
					values[j].jslider.setValue((int)( 1000.0D * ((values[j].current - values[j].start) / (values[j].end - values[j].start)) ));
				}
			}
		//}

	}

        //   'PlotScaleValues' is an jradSliderFrame internal class
	class PlotScaleValues {

            double wminplot;
            double wmaxplot;
            double plotmin;
            double plotmax;

            JTextField jtextfieldWminplot;
            JTextField jtextfieldWmaxplot;
            JTextField jtextfieldPlotmin;
            JTextField jtextfieldPlotmax;

            // constructor without values
            public void PlotScaleValues() {

                //set default graph values
                wminplot = 0.15;
                wmaxplot = 5.10;
                plotmin = 0.00;
                plotmax = 0.75;

                PlotScaleValues(wminplot, wmaxplot, plotmin, plotmax);
        }

            // constructor with values
            public void PlotScaleValues(double wminplot, double wmaxplot, double plotmin, double plotmax) {

                this.wminplot = wminplot;
                this.wmaxplot = wmaxplot;
                this.plotmin = plotmin;
                this.plotmax = plotmax;

                //check for bad axis settings
                if (wmaxplot <= wminplot) wmaxplot = wminplot + 0.001;
                if (plotmax <= plotmin) plotmax = plotmin + 0.001;

                this.jtextfieldWminplot = new JTextField(String.format("%8.7f", wminplot), 8);
                this.jtextfieldWminplot.setEditable(true);

                this.jtextfieldWmaxplot = new JTextField(String.format("%8.7f", wmaxplot), 8);
                this.jtextfieldWmaxplot.setEditable(true);

                this.jtextfieldPlotmin = new JTextField(String.format("%8.7f", plotmin), 8);
                this.jtextfieldPlotmin.setEditable(true);

                this.jtextfieldPlotmax = new JTextField(String.format("%8.7f", plotmax), 8);
                this.jtextfieldPlotmax.setEditable(true);
            }
        }


//   'Values' is an jradSliderFrame internal class
	class Values {

		int initvalue;

		String titles;
		double start;
		double end;
		double current;
		JLabel jlable;
		JTextField jtextfieldStart;
		JTextField jtextfieldEnd;
		JTextField jtextfieldCurrent;
		JSlider jslider;
		String materialType;

		// constructor without values
		public Values() {
			titles = "";
			start = 0.0;
			end = 0.0;
			current = 0.0;

			jlable = new JLabel(titles);

			jtextfieldStart = new JTextField(8);
			jtextfieldStart.setEditable(true);

			jtextfieldEnd = new JTextField(8);
			jtextfieldEnd.setEditable(true);

			jtextfieldCurrent = new JTextField(8);
			jtextfieldCurrent.setEditable(true);

			jslider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 0);
			materialType = "";
		}

		// constructor with values
		public Values(String titles, double start, double end, double current, String materialType) {
			this.titles = titles;
			this.start = start;
			this.end = end;
			this.current = current;

			this.jlable = new JLabel(titles);

			this.jtextfieldStart = new JTextField(String.format("%8.7f", start), 8);
			this.jtextfieldStart.setEditable(true);

			this.jtextfieldEnd = new JTextField(String.format("%8.7f", end), 8);
			this.jtextfieldEnd.setEditable(true);

			this.jtextfieldCurrent = new JTextField(String.format("%8.7f", current), 8);
			this.jtextfieldCurrent.setEditable(true);

			initvalue = (int) (1000.0 * (current - start) / (end - start));
			this.jslider = new JSlider(JSlider.HORIZONTAL, 0, 1000, initvalue);

			//materialType = abundance, grainsize, mixture
			this.materialType = materialType;
		}
	}


	class Strout {
		//  print values to format output (to feed into Radtran)

                String[] cntrlstr = new String[115];

		public Strout(Values[] values) {

		int iv = 0;

		// USER CUSTOMIZATION;
		// overlay reference spectra: change ov1, ov2, ov3 to point to the spectra you are trying to fit;

		cntrlstr[0] = "x";
		cntrlstr[1] = "e";
		cntrlstr[2] = "x";
		cntrlstr[3] = "ov2=u365  U265   \\# blue   B ring cassini rev75 REFL ed UVIS boxsm3   512";   
		cntrlstr[4] = "ov3=u357  U319   \\# green  B-ring sph-albedo smthr hst cuzzi2018 s1  1885";
		cntrlstr[5] = "ov1=u312  U6     \\# red    ring B vims S01-45RC19 886,989,21882p s1   352";
		cntrlstr[6] = "ov3=on";
		cntrlstr[7] =  "\\# future                               ";
		cntrlstr[8] =  "\\# future                               ";
		cntrlstr[9] =  "\\# future                               ";
		cntrlstr[10] = "\\# future                               ";
		cntrlstr[11] = "\\# future                               ";
		cntrlstr[12] = "\\# future                               ";
		cntrlstr[13] = "\\# future                               ";
		cntrlstr[14] = "\\# future                               ";

		cntrlstr[15] = "R";
		cntrlstr[16] = "1";
		cntrlstr[17] = "V6                     \\# wavelengths to 0.1 - 6 micron 3800ch      3800";    
		cntrlstr[18] = "                      \\# deleted channels";
		cntrlstr[19] = "10                    \\# number of components";

		// USER CUSTOMIZATION;
		// customize which optical constants you want to use;

// material 1
		iv = 0;
		cntrlstr[20] = "s                     \\# material 1";
		cntrlstr[21] = "v29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800";
		cntrlstr[22] = "v40                   \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800";
		//System.o5t.printf ("%10.7f  %10.7f  5  \\# gr up= .3-2um up\n", values[0].current, values[1].current);
		//cntrlstr[14] = new String("%10.7f  %10.7f  5  \\# gr up= .3-2um up\n", values[0].current, values[1].current);
		//cntrlstr[14] = new String(values[0].current + " " + values[1].current) + " " + 5 + "  \\# gr up= .3-2um up";

		cntrlstr[23] = String.format("%10.7f  %10.7f  5  \\# gr up= .3-2um up", values[iv].current, values[iv+1].current);

		cntrlstr[24] = "\\##################";
// material 2
		iv = 2;
		cntrlstr[25] = "s                     \\# material 2";
		cntrlstr[26] = "v73                   \\# Triton Tholin INDXREF (dale Ore 3/2011)   3800";
		cntrlstr[27] = "v84                   \\# Triton Tholin ABSCOEF (Dale Ore 3/2011)   3800";
		cntrlstr[28] = String.format("%10.7f  %10.7f  5  \\# grain size in cm, abundance, density", values[iv].current, values[iv+1].current);
		cntrlstr[29] = "\\##################";
// material 3
// KEL 06/07/18		iv = 5;
                iv = 4;
		cntrlstr[30] = "f 2                   \\# material 3";
		cntrlstr[31] = "v117                  \\# nanoHematite2+0.03 CO2 INDX REF intrpuv1  3800       ";
		cntrlstr[32] = "v142                  \\# CO2 ICE, frm Warren INDX REF extraptoUV   3800       ";
		cntrlstr[33] = "v128   f              \\# nanoHematite2.5+0.03CO2+.1nFe ABS COEF e  3800       ";
// KEL 06/27/18		cntrlstr[26] = String.format("v153  %10.7f       \\# CO2 ice (Warren) ABS COEF extraptoUV      3800    ", values[4].current);
// KEL 06/27/18		cntrlstr[27] = String.format("%10.7f  %10.7f  5  \\# grain size in cm, abundance, density", values[iv].current, values[iv+1].current);
		cntrlstr[34] = String.format("v153  %10.7f       \\# CO2 ice (Warren) ABS COEF extraptoUV      3800    ", values[iv].current);
		cntrlstr[35] = String.format("%10.7f  %10.7f  5  \\# grain size in cm, abundance, density", values[iv+1].current, values[iv+2].current);
		cntrlstr[36] = "\\##################";
// material 4
		iv = 7;
		cntrlstr[37] = "f 2                   \\# material 4";
		cntrlstr[38] = "v117                  \\# nanoHematite2+0.03 CO2 INDX REF intrpuv1  3800       ";
		cntrlstr[39] = "v142                  \\# CO2 ICE, frm Warren INDX REF extraptoUV   3800       ";
		cntrlstr[40] = "v128   f              \\# nanoHematite2.5+0.03CO2+.1nFe ABS COEF e  3800       ";  
		cntrlstr[41] = String.format("v153  %10.7f       \\# CO2 ice (Warren) ABS COEF extraptoUV   3800       ", values[iv].current);
		cntrlstr[42] = String.format("%10.7f  %10.7f  5  \\# grain size in cm, abundance, density", values[iv+1].current, values[iv+2].current);
		cntrlstr[43] = "\\##################";
// material 5
		iv = 10;
		cntrlstr[44] = "b 3                   \\# material 5";
		cntrlstr[45] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800";
		cntrlstr[46] = "v73                   \\# Triton Tholin INDXREF (dale Ore 3/2011)   3800";
		cntrlstr[47] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800       ";
		cntrlstr[48] = "v62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800";
		cntrlstr[49] = String.format("v84   %10.7f       \\# Triton Tholin ABSCOEF (Dale Ore 3/2011)   3800", values[iv].current);
		cntrlstr[50] = String.format("v363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800    ", values[iv+1].current);
		cntrlstr[51] = String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current);
		cntrlstr[52] = "\\##################";
// material 6
		iv = 14;
		cntrlstr[53] = "b 3                   \\# material 6";
		cntrlstr[54] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800";
		cntrlstr[55] = "v73                   \\# Triton Tholin INDXREF (dale Ore 3/2011)   3800";
		cntrlstr[56] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800       ";
		cntrlstr[57] = "v62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800";
		cntrlstr[58] = String.format("v84   %10.7f       \\# Triton Tholin ABSCOEF (Dale Ore 3/2011)   3800", values[iv].current);
		cntrlstr[59] = String.format("v363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800    ", values[iv+1].current);
		cntrlstr[60] = String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current);
		cntrlstr[61] = "\\##################";
// material 7
		iv = 18;
		cntrlstr[62] = "b 3                   \\# material 7";
		cntrlstr[63] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800";
		cntrlstr[64] = "v73                   \\# Triton Tholin INDXREF (dale Ore 3/2011)   3800";
		cntrlstr[65] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800       ";
		cntrlstr[66] = "v62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800";
		cntrlstr[67] = String.format("v84   %10.7f       \\# Triton Tholin ABSCOEF (Dale Ore 3/2011)   3800", values[iv].current);
		cntrlstr[68] = String.format("v363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800    ", values[iv+1].current);
		cntrlstr[69] = String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current);
		cntrlstr[70] = "\\##################";

// material 8
		iv = 22;
		cntrlstr[71] = "b 3                   \\# material 8";
		cntrlstr[72] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800";
		cntrlstr[73] = "v73                   \\# Triton Tholin INDXREF (dale Ore 3/2011)   3800";
		cntrlstr[74] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800       ";
		cntrlstr[75] = "v62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800";
		cntrlstr[76] = String.format("v84   %10.7f       \\# Triton Tholin ABSCOEF (Dale Ore 3/2011)   3800", values[iv].current);
		cntrlstr[77] = String.format("v363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800    ", values[iv+1].current);
		cntrlstr[78] = String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current);
		cntrlstr[79] = "\\##################";

// material 9
		iv = 26;
		cntrlstr[80] = "b 3                   \\# material 9";
		cntrlstr[81] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800";
		cntrlstr[82] = "v73                   \\# Triton Tholin INDXREF (dale Ore 3/2011)   3800";
		cntrlstr[83] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800       ";
		cntrlstr[84] = "v62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800";
		cntrlstr[85] = String.format("v84   %10.7f       \\# Triton Tholin ABSCOEF (Dale Ore 3/2011)   3800", values[iv].current);
		cntrlstr[86] = String.format("v363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800    ", values[iv+1].current);
		cntrlstr[87] = String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current);
		cntrlstr[88] = "\\##################";

// material 10
		iv = 30;
		cntrlstr[89] = "b 3                   \\# material 10";
		cntrlstr[90] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800";
		cntrlstr[91] = "v29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800";
		cntrlstr[92] = "v51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800       ";
		cntrlstr[93] = "v62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800";
		cntrlstr[94] = String.format("v84   %10.7f       \\# Triton Tholin ABSCOEF (Dale Ore 3/2011)   3800", values[iv].current);
		cntrlstr[95] = String.format("v363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800    ", values[iv+1].current);
		cntrlstr[96] = String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current);
		cntrlstr[97] = "\\##################";

		cntrlstr[98] = " 0 20 20              \\# placeholder for spherical albedo";
		cntrlstr[99] = "y";
		cntrlstr[100] = "                      \\# s value";
		cntrlstr[101] = "                      \\# wavelength dependent scattering";
		cntrlstr[102] = "                      \\# no nf";
		cntrlstr[103] = "0 \\# 2               \\# FUTURE, IF NEEDED";
		cntrlstr[104] = "\\# d1306   0.00001   \\# FUTURE, NEED 1023ch: 6C.01nFe.02Fe.917HemnFe.04nHnFe.H2O+CO2    352";
		cntrlstr[105] = "\\# d1312   0.00001   \\# FUTURE, NEED 1023ch: H2Oice+nFe+nHem mol mix+areal.1 10CMPREF   352";
		cntrlstr[106] = "\\#       1111111111222222222233333333334";
		cntrlstr[107] = "\\#34567890123456789012345678901234567890";
		cntrlstr[108] = String.format("10-component radtran, set title here     w%8.5f %8.5f %8.5f %8.5f",
		    plotScaleValues.wminplot, plotScaleValues.wmaxplot, plotScaleValues.plotmin, plotScaleValues.plotmax);
		cntrlstr[109] = "bd1= 1.04u_s=.017 W  0.958   0.986   1.02     1.05      1.080   1.110";
		cntrlstr[110] = "bd2= 1.25u_s=.066 W  1.12    1.16    1.25     1.29      1.34    1.38";
		cntrlstr[111] =  "bd3= 1.5um_s=.61  W  1.35    1.41    1.50     1.55      1.76    1.81";
		cntrlstr[112] = "bd4= 2.0um_s=.78  W  1.79    1.845   2.01     2.04      2.22    2.259";
		cntrlstr[113] = "\\# future                               ";
		cntrlstr[114] = "showbd";

		}
	}

        Values[] InitializeValues() {
	//Values[] InitializeValues(Values[] values) {
            
            String titles;
            double start;
            double end;
            double current;
            String materialType;   //materialType = abundance, grainsize, mixture

            // ---------------------------------------------------------------------------------
            //  Initialize slider values (replace later with text file restart input)

/*  One way of intializing the values array (only these 4 variables) */
/*
		values[0] = new Values();
		values[0].titles   = new String("M1 Fe__ gs: ");
		values[0].start    = 0.0000001;   // iron (Fe) grain size in cm ;
		values[0].end      = 0.0002000;
		values[0].current  = 0.0000119;  // last best
*/

/*  Another way of intializing the values array (with variables, JLabel, JTextFields, and JSlider ) */

            int numSliders = 34;
            Values[] values = new Values[numSliders];

	// USER CUSTOMIZATION;
	// customize the starting abundances and grain sizes ;

	// Material 1 iron (Fe) ;

		titles    = "M01 Fe__ gs: ";
		start     = 0.0000001;   // iron (Fe) grain size in cm ;
		end       = 0.0000200;
		current   = 0.0000181;  // last best
		materialType = "grainsize";
		values[0] = new Values(titles, start, end, current, materialType);

		titles    = "M01 Fe__ ab: ";
		start     = 0.0000000;     // iron (Fe) abundance
		end       = 0.0002000;
		current   = 0.0000000;
		materialType = "abundance";
		values[1] = new Values(titles, start, end, current, materialType);

	// Material 2

		titles   = "<html><hr><font color='#4444ff'>M02 thol gs: </font></html>";
		start    = 0.00000010;   //  ;
		end      = 0.00010000;
		current  = 0.00000020;  // last best
		materialType = "grainsize";
		values[2] = new Values(titles, start, end, current, materialType);

		titles   = "<html><font color='#4444ff'>M02 thol ab: </font></html>";
		start    = 0.00000000;     // 
		end      = 0.05000000;
		current  = 0.00000000;
		materialType = "abundance";
		values[3] = new Values(titles, start, end, current, materialType);

	// Material 3    2-component mix  nano hematite + CO2

		titles   = "<html><hr><font color='#cc3333'>M03 CO2_ mx: </font></html>";
		start    = 0.00000000;
		end      = 0.01000000;
		current  = 0.00000000;   // amount of CO2 in hematite
		materialType = "mixture";
		values[4] = new Values(titles, start, end, current, materialType);

		titles   = "M03 nHem gs: ";
		start    = 0.00000010;   // nanohematite grain size in cm ;
		end      = 0.00500000;
		current  = 0.00200000;
		materialType = "grainsize";
		values[5] = new Values(titles, start, end, current, materialType);

		titles   = "M03 nHem ab: ";
		start    = 0.00000000;     // nanohematite abundance
		end      = 0.00500000;
		current  = 0.00200000;
		materialType = "abundance";
		values[6] = new Values(titles, start, end, current, materialType);

	// Material M4    2-component mix  nano hematite + H2O ice

		titles   = "<html><hr><font color='#cc3333'>M04 CO2_ mx: </font></html>";
		start    = 0.00000000;
		end      = 0.00300000;
		current  = 0.00000000;   // amount of hematite in ice
		materialType = "mixture";
		values[7] = new Values(titles, start, end, current, materialType);

		titles   = "M04 nHem gs: ";
		start    = 0.00000010;   // H2O Ice ;
		end      = 0.00100000;
		current  = 0.00026410;
		materialType = "grainsize";
		values[8] = new Values(titles, start, end, current, materialType);

		titles   = "M04 nHem ab: ";
		start    = 0.00000000;     // Hem ;
		end      = 0.70100000;
		current  = 0.00000000;
		materialType = "abundance";
		values[9] = new Values(titles, start, end, current, materialType);

	// Material M5    2-component mix  nano iron + H2O ice

		titles  = "<html><hr><font color='#cc3333'>M05 Thol mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0001000;
		current = 0.0000278;   // amount of iron in ice
		materialType = "mixture";
		values[10] = new Values(titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>M05 D-O_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;   // amount of DO  in ice
		materialType = "mixture";
		values[11] = new Values(titles, start, end, current, materialType);

		titles  = "M05 H2O_ gs: ";
		start   = 0.0000001;   // H2O grain size in cm ;
		end     = 0.1000000;
		current = 0.0244001;
		materialType = "grainsize";
		values[12] = new Values(titles, start, end, current, materialType);

		titles  = "M05 H2O_ ab: ";
		start   = 0.0000000;     // H2O  abundance
		end     = 0.9999900;
		current = 0.0000000;
		materialType = "abundance";
		values[13] = new Values(titles, start, end, current, materialType);

	// Material M6    2-component mix  nano iron + H2O ice

		titles  = "<html><hr><font color='#cc3333'>M06 Thol mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0010000;
		current = 0.0000137;   // amount of iron in ice
		materialType = "mixture";
		values[14] = new Values(titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>M06 D-O_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;   // amount of DO  in ice
		materialType = "mixture";
		values[15] = new Values(titles, start, end, current, materialType);

		titles  = "M06 H2O_ gs: ";
		start   = 0.0000001;   // ice grain size in cm ;
		end     = 0.0400000;
		current = 0.0132600;
		materialType = ("grainsize");
		values[16] = new Values(titles, start, end, current, materialType);

		titles  = "M06 H2O_ ab: ";
		start   = 0.0000000;     // ice abundance
		end     = 0.9000000;
		current = 0.6761285;
		materialType = "abundance";
		values[17] = new Values(titles, start, end, current, materialType);

	// Material M7    2-component mix  nano iron + H2O ice

		titles  = "<html><hr><font color='#cc3333'>M07 Thol mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0000000;   // amount of iron in ice
		materialType = "mixture";
		values[18] = new Values(titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>M07 D-O_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;   // amount of DO  in ice
		materialType = "mixture";
		values[19] = new Values(titles, start, end, current, materialType);

		titles  = "M07 H2O_ gs: ";
		start   = 0.0000001;   // ice grain size in cm ;
		end     = 0.0400000;
		current = 0.0050801;
		materialType = "grainsize";
		values[20] = new Values(titles, start, end, current, materialType);

		titles  = "M07 H2O_ ab: ";
		start   = 0.0000000;     // ice abundance
		end     = 0.4000000;
		current = 0.2501058;
		materialType = "abundance";
		values[21] = new Values(titles, start, end, current, materialType);

	// Material M8    2-component mix  nano iron + H2O ice

		titles  = "<html><hr><font color='#cc3333'>M08 Thol mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0010000;
		current = 0.0000630;   // amount of iron in ice
		materialType = "mixture";
		values[22] = new Values(titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>M08 D-O_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;   // amount of DO  in ice
		materialType = "mixture";
		values[23] = new Values(titles, start, end, current, materialType);

		titles  = "M08 H2O_ gs: ";
		start   = 0.0000001;   // ice grain size in cm ;
		end     = 0.0032000;
		current = 0.0015297;
		materialType = "grainsize";
		values[24] = new Values(titles, start, end, current, materialType);

		titles  = "M08 H2O_ ab: ";
		start   = 0.0000000;     // ice abundance
		end     = 0.1000000;
		current = 0.0659037;
		materialType = "abundance";
		values[25] = new Values(titles, start, end, current, materialType);

	// Material M9    2-component mix  nano iron + H2O ice

		titles  = "<html><hr><font color='#cc3333'>M09 Thol mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0013150;   // amount of iron in ice
		materialType = "mixture";
		values[26] = new Values(titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>M09 D-O_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;   // amount of DO  in ice
		materialType = "mixture";
		values[27] = new Values(titles, start, end, current, materialType);

		titles  = "M09 H2O_ gs: ";
		start   = 0.0000001;   // ice grain size in cm ;
		end     = 0.0002000;
		current = 0.0000499;
		materialType = "grainsize";
		values[28] = new Values(titles, start, end, current, materialType);

		titles  = "M09 H2O_ ab: ";
		start   = 0.0000000;     // ice abundance
		end     = 0.0020000;
		current = 0.0005400;
		materialType = "abundance";
		values[29] = new Values(titles, start, end, current, materialType);

	// Material M10    2-component mix  nano iron + H2O ice

		titles  = "<html><hr><font color='#cc3333'>M10 Thol mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0000000;   // amount of iron in ice
		materialType = "mixture";
		values[30] = new Values(titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>M10 D-O_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;   // amount of DO  in ice
		materialType = "mixture";
		values[31] = new Values(titles, start, end, current, materialType);

		titles  = "M10 H2O_ gs: ";
		start   = 0.0000001;   // ice grain size in cm ;
		end     = 0.0002000;
		current = 0.0000605;
		materialType = "grainsize";
		values[32] = new Values(titles, start, end, current, materialType);

		titles  = "M10 H2O_ ab: ";
		start   = 0.0000000;     // ice abundance
		end     = 0.0200000;
		current = 0.0073220;
		materialType = "abundance";
		values[33] = new Values(titles, start, end, current, materialType);


                return values;
        }

}



/**
 *
 * @author elivo
 * from rainbow-src_util/Utility.java
 */
class Utility {
    
    public Utility() {
    
    }

    //get screen size
    public static Dimension getScreenSize() {
        Dimension scrnSize = Toolkit.getDefaultToolkit().getScreenSize();
        //scrnSize.getHeight();
        //scrnSize.getWidth();
          // or
        //int scrnHeight = scrnSize.height;
        //int scrnWidth = scrnSize.width;

        return scrnSize;
    }
}
