package jradtrans;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.ArrayList;
import javax.swing.*;
import javax.swing.event.*;

public class radtran_j21L2_16c_c {

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

	int numSliders;
	Values[] values;
	PlotScaleValues plotScaleValues;
	Strout strout;
	String cwdJChooser = ".";

	boolean fromtextbox = false;
	double scale = 0.00001;

	int modelcount = 0;

	JButton exitbutton;     //terminate Radtran program (GUI and executable program)
        JButton savespecprbutton;     //save current radtran model to specpr file record
        JButton savemodelbutton;     //save current radtran model to text file
        JButton readbutton;     //read new radtran model from text file
        JButton spacebutton;
        String spacebuttonTitle = null;  //title of filename read in
        //JButton spacebutton2;  //not used
        //String spacebuttonTitle2= null;
        JButton scaleTitleButton;
        String scalebuttonTitle = "Plot Scale:";

        private String title = null;
        private String spdWrite = null;

	public jradSliderFrame() {

                values = InitializeValues();

        //  Setup GUI interface
                Dimension scrnSize = Utility.getScreenSize();
                System.out.println("\\# screen dimensions = " + scrnSize.width + " x " + scrnSize.height);

            //set jFrame size variables to current jFrame size relative to screen dimensions
            final int DEFAULT_WIDTH;
            final int DEFAULT_HEIGHT;
            int tmpSrcnWidth = 500;
            int tmpSrcnHeight = 1247;

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
                    //for (int i = 0; i < values.length; i++) {
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

            //  Save Specpr spectrum Program Button
//NOTE: not working yet - commands script has changed
		savespecprbutton = new JButton("Save SpecPR spectrum");
		savespecprbutton.addActionListener(this);
		commandPanel.add(savespecprbutton);
                savespecprbutton.setEnabled(false);

            //  Save Model Program Button

		savemodelbutton = new JButton("Save Text Model");
		savemodelbutton.addActionListener(this);
		commandPanel.add(savemodelbutton);

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

/*  not used
                //spacer for button width
                spacebutton2 = new JButton(spacebuttonTitle2);
                commandPanel.add(spacebutton2);
*/
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

        // test for Program Save Spectrum to SPDfile
		if (actsource == savespecprbutton) {
                      //write save (model) to specpr SPDfile

                      //set spd spectrum title
                        //String strOutput = JOptionPane.showInputDialog(null, "Enter Model Title");
                        String strOutput = JOptionPane.showInputDialog(null, "Enter Model Title", getTitle());
                        String strOutputTrimmed = null;
                        String strFileLetterTrimmed = null;

                        if ((strOutput != null) &&( strOutput.length() > 0)) strOutputTrimmed = strOutput.trim();
                        if ((strOutputTrimmed != null) &&(strOutputTrimmed.length() > 40)) strOutputTrimmed = strOutputTrimmed.substring(0, 40);

                        //if (!strOutputTrimmed.matches(".*\\S.*")) strOutputTrimmed = null;
                        //if (!strOutputTrimmed.matches("^[\\p{IsAlphabetic}\\p{IsDigit}]+$")) strOutputTrimmed = null;

                        //good title
                        if ((strOutputTrimmed != null) &&( strOutputTrimmed.length() > 0)) {
                            setTitle(strOutputTrimmed);

                            strFileLetterTrimmed = null;
                            int dialogResult = 0;
                            while (dialogResult == 0) {
                                //String strFileLetter = JOptionPane.showInputDialog(null, "Enter SPD File Letter");
                                String strFileLetter = JOptionPane.showInputDialog(null, "Enter SPD File Letter", getSPDwrite());
                                strFileLetterTrimmed = null;
                                if ((strFileLetter != null) &&( strFileLetter.length() > 0)) strFileLetterTrimmed = strFileLetter.trim().toLowerCase();

                                if ((strFileLetterTrimmed != null) &&( strFileLetterTrimmed.length() > 0)) {
                                    if ((strFileLetterTrimmed.length() == 1) && (strFileLetterTrimmed.matches("^[duvwy]+$"))) {
                                        setSPDwrite(strFileLetterTrimmed);
                                        dialogResult = 1;
                                    } else {
                                        dialogResult = JOptionPane.showConfirmDialog(null, "Incorrect File Letter, (Letter Selection: duvwy) Try Again ?", title, WIDTH);
                                        strFileLetterTrimmed = null;
                                        if (dialogResult > 0) strOutputTrimmed = null;  //Yes,No,Cancel:0,1,2)
                                    }
                                } else {
                                    strFileLetterTrimmed = null;
                                    strOutputTrimmed = null;
                                    dialogResult = 1;
                                }
                            }
                        }

                        if ((strOutputTrimmed != null)  &&( strOutputTrimmed.length() > 0)) {

                            //cycle one time to set title
                            System.out.println("x"); 
                            System.out.println("x"); 
                            printall();

                            //save model spectrum and history records using e for save exit and SPD letters w & w
                            System.out.println("e");    //("e" soft exit writes radtran model specpr spectrum to spdfile)
                          // Note: getSPDwrite set in: class Strout as model is created
                            System.out.println(getSPDwrite());    //(spectrum record written to SPDfile 'w')
                            //System.out.println("w");    //(spectrum record written to SPDfile 'w')
                            System.out.println(getSPDwrite());    //(following text history file written to SPDfile 'w'; program returns to main menu)
                            //System.out.println("w");    //(following text history file written to SPDfile 'w'; program returns to main menu)

               		  //System.out.println("e");    //("e" writes radtran model specpr spectrum to spdfile & returns to main menu)
                          //                            //(rather than "x" used to NOT write specpr spectrum to spdfile)
                            System.out.println("e");    //(backup e return to radtran main menu)
                            System.out.println("x");    //(backup x return to radtran main menu)

                        //bad title or SpecPR associate file letter
                        } else {
                            //no spectrum model record writen
                            JOptionPane.showMessageDialog(null, "Model NOT Written", null, JOptionPane.WARNING_MESSAGE);
                        }
		}

	// test for Program Save Model
		if (actsource == savemodelbutton) {
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
                                                for (int ip = 0; ip < strout.cnt.size(); ip++) {
                                                    fw.write(strout.cnt.get(ip) + "\n");
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

		for (int ip = 0; ip < strout.cnt.size(); ip++) {
			System.out.println(strout.cnt.get(ip));
		}

		try {
			Thread.sleep(85);
		} catch (InterruptedException e) {
			// wake up early
		}

	}


	public void normalizeAbundances(int in) {
		int i = in;
                int layerArg = values[i].layer;

		//if (abundBoolean) {
			double abundTotal = 0;
			double abundScale = 0;
			for (int j = 0; j < values.length ; j++) {
				if((values[j].materialType.equals("abundance")) && (values[j].layer == layerArg)) {
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
				if ( (i != j) && (values[j].materialType.equals("abundance")) && (values[j].layer == layerArg) ) {
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

                //String op;
                int layer;
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
                        //op = "";
                        layer = 0;
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
		public Values(int layer, String titles, double start, double end, double current, String materialType) {
                //public Values(String op, String titles, double start, double end, double current, String materialType) {
                        //this.op = op;
                        this.layer = layer;
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

                ArrayList<String> cnt = new ArrayList<String>();
                //String[] cntrlstr = new String[215];  //in future: convert to String List

		public Strout(Values[] values) {

		int iv = 0;

// !!!!! NOTE: class Strout and method InitializeValues() are the ONLY user modifiable regions !!!!!

		// USER CUSTOMIZATION;
		// overlay reference spectra: change ov1, ov2, ov3 to point to the spectra you are trying to fit;

                if (getSPDwrite() == null)
                    setSPDwrite("w");  //specpr restartfile (r1) assignment letter for spectrum writes
                if (getRadGUITitle() == null)
                  setRadGUITitle("Radtran java 10-component gui");  //output title in model




                //cnt.add();
                cnt.add("x");
                cnt.add("e");
                cnt.add("x");

                cnt.add("\\#            d = /p1/rclark/opticalconstants-a/sps0000");
                cnt.add("\\#            u = /p1/rclark/vims18-1/sps0013");
                cnt.add("\\#            v = a local specpr file");

                cnt.add("ov3=u258  U6           \\# red   rings_s42r751atphasepnlrc17 341pN B avg   352");
                cnt.add("ov2=u269  U265         \\# blue  B ring cassini rev75 REFL ed UVIS         512");
                cnt.add("ov1=262   U6           \\# green iapetus.s33r49Bqltrc17 98pxL185S292drksc  352");
                cnt.add("ov3=on");

                cnt.add("\\# future                               ");

                cnt.add("L2                    \\# 2-layer model  (add a V for verbose output, L2 V)");
                cnt.add("1                     \\# Reflectance calculation");
                cnt.add("D6                    \\# wavelengths to 0.1 - 6 micron 3800ch      3800");
                cnt.add("                      \\# deleted channels");

                iv = 0;
                cnt.add(String.format("%10.7f  \\# layer 1 thickness in cm", values[iv].current));
                cnt.add(String.format("\\#thickness  %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add("8                     \\# Layer 1 number of components");
                cnt.add("8                     \\# Layer 2 number of components");

		// USER CUSTOMIZATION;
		// customize which optical constants you want to use;

                cnt.add("\\# Radtran Mixtures");
// LAYER 1
// material 1
                iv = 1;
                cnt.add("\\##################    Layer 1");
                cnt.add("s                     \\# material 1");
                cnt.add("d29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800");
                cnt.add("d40                   \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800");
                cnt.add(String.format("%10.7f  %10.7f  5  \\# gr up= .3-2um up", values[iv].current, values[iv+1].current));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add("\\##################    Layer 1");
// material 2
		iv = 3;
                cnt.add("s                     \\# material 2");
                cnt.add("d73                   \\# Triton Tholin INDXREF (dale Ore 3/2011)   3800");
                cnt.add("d84                   \\# Triton Tholin ABSCOEF (Dale Ore 3/2011)   3800");
                cnt.add(String.format("%10.7f  %10.7f  5  \\# grain size in cm, abundance, density", values[iv].current, values[iv+1].current));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add("\\##################    Layer 1");
// material 3
                iv = 5;
                cnt.add("f 2                   \\# material 3");
                cnt.add("d117                  \\# nanoHematite2+0.03 CO2 INDX REF intrpuv1  3800");
                cnt.add("d142                  \\# CO2 ICE, frm Warren INDX REF extraptoUV   3800");
                cnt.add("d128   f              \\# nanoHematite2.5+0.03CO2+.1nFe ABS COEF e  3800");
                cnt.add(String.format("d153  %10.7f       \\# CO2 ice (Warren) ABS COEF extraptoUV      3800", values[iv].current));
                cnt.add(String.format("%10.7f  %10.7f  5  \\# grain size in cm, abundance, density", values[iv+1].current, values[iv+2].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add("\\##################    Layer 1");
// material 4
		iv = 8;
                cnt.add("f 2                   \\# material 4");
                cnt.add("d117                  \\# nanoHematite2+0.03 CO2 INDX REF intrpuv1  3800");
                cnt.add("d142                  \\# CO2 ICE, frm Warren INDX REF extraptoUV   3800");
                cnt.add("d128   f              \\# nanoHematite2.5+0.03CO2+.1nFe ABS COEF e  3800");
                cnt.add(String.format("d153  %10.7f       \\# CO2 ice (Warren) ABS COEF extraptoUV      3800", values[iv].current));
                cnt.add(String.format("%10.7f  %10.7f  5  \\# grain size in cm, abundance, density", values[iv+1].current, values[iv+2].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add("\\##################    Layer 1");
// material 5
		iv = 11;
                cnt.add("b 3                   \\# material 5");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800");
                cnt.add(String.format("d40   %10.7f       \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800", values[iv].current));
                cnt.add(String.format("d363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800", values[iv+1].current));
                cnt.add(String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+3].titles, values[iv+3].start, values[iv+3].end));
                cnt.add("\\##################    Layer 1");
// material 6
		iv = 15;
                cnt.add("b 3                   \\# material 6");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800");
                cnt.add(String.format("d40   %10.7f       \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800", values[iv].current));
                cnt.add(String.format("d363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800", values[iv+1].current));
                cnt.add(String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+3].titles, values[iv+3].start, values[iv+3].end));
                cnt.add("\\##################    Layer 1");
// material 7
		iv = 19;
                cnt.add("b 3                   \\# material 7");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800");
                cnt.add(String.format("d40   %10.7f       \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800", values[iv].current));
                cnt.add(String.format("d363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800", values[iv+1].current));
                cnt.add(String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+3].titles, values[iv+3].start, values[iv+3].end));
                cnt.add("\\##################    Layer 1");
// material 8
		iv = 23;
                cnt.add("b 3                   \\# material 8");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800");
                cnt.add(String.format("d40   %10.7f       \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800", values[iv].current));
                cnt.add(String.format("d363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800", values[iv+1].current));
                cnt.add(String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+3].titles, values[iv+3].start, values[iv+3].end));
                cnt.add("\\##################    Layer 1");

// LAYER 2
// material 1
                iv = 27;
                cnt.add("\\##################    Layer 2");
                cnt.add("s                     \\# material 1");
                cnt.add("d29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800");
                cnt.add("d40                   \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800");
                cnt.add(String.format("%10.7f  %10.7f  5  \\# gr up= .3-2um up", values[iv].current, values[iv+1].current));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add("\\##################    Layer 2");
// material 2
		iv = 29;
                cnt.add("s                     \\# material 2");
                cnt.add("d73                   \\# Triton Tholin INDXREF (dale Ore 3/2011)   3800");
                cnt.add("d84                   \\# Triton Tholin ABSCOEF (Dale Ore 3/2011)   3800");
                cnt.add(String.format("%10.7f  %10.7f  5  \\# grain size in cm, abundance, density", values[iv].current, values[iv+1].current));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add("\\##################    Layer 2");
// material 3
                iv = 31;
                cnt.add("f 2                   \\# material 3");
                cnt.add("d117                  \\# nanoHematite2+0.03 CO2 INDX REF intrpuv1  3800");
                cnt.add("d142                  \\# CO2 ICE, frm Warren INDX REF extraptoUV   3800");
                cnt.add("d128   f              \\# nanoHematite2.5+0.03CO2+.1nFe ABS COEF e  3800");
                cnt.add(String.format("d153  %10.7f       \\# CO2 ice (Warren) ABS COEF extraptoUV      3800", values[iv].current));
                cnt.add(String.format("%10.7f  %10.7f  5  \\# grain size in cm, abundance, density", values[iv+1].current, values[iv+2].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add("\\##################    Layer 2");
// material 4
                iv = 34;
                cnt.add("f 2                   \\# material 4");
                cnt.add("d117                  \\# nanoHematite2+0.03 CO2 INDX REF intrpuv1  3800");
                cnt.add("d142                  \\# CO2 ICE, frm Warren INDX REF extraptoUV   3800");
                cnt.add("d128   f              \\# nanoHematite2.5+0.03CO2+.1nFe ABS COEF e  3800");
                cnt.add(String.format("d153  %10.7f       \\# CO2 ice (Warren) ABS COEF extraptoUV      3800", values[iv].current));
                cnt.add(String.format("%10.7f  %10.7f  5  \\# grain size in cm, abundance, density", values[iv+1].current, values[iv+2].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add("\\##################    Layer 2");
// material 5
		iv = 37;
                cnt.add("b 3                   \\# material 5");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800");
                cnt.add(String.format("d40   %10.7f       \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800", values[iv].current));
                cnt.add(String.format("d363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800", values[iv+1].current));
                cnt.add(String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+3].titles, values[iv+3].start, values[iv+3].end));
                cnt.add("\\##################    Layer 2");
// material 6
		iv = 41;
                cnt.add("b 3                   \\# material 6");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800");
                cnt.add(String.format("d40   %10.7f       \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800", values[iv].current));
                cnt.add(String.format("d363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800", values[iv+1].current));
                cnt.add(String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+3].titles, values[iv+3].start, values[iv+3].end));
                cnt.add("\\##################    Layer 2");
// material 7
		iv = 45;
                cnt.add("b 3                   \\# material 7");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800");
                cnt.add(String.format("d40   %10.7f       \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800", values[iv].current));
                cnt.add(String.format("d363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800", values[iv+1].current));
                cnt.add(String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+3].titles, values[iv+3].start, values[iv+3].end));
                cnt.add("\\##################    Layer 2");
// material 8
		iv = 49;
                cnt.add("b 3                   \\# material 8");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d29                   \\# Iron (Fe) INDX REF Palik+Ordal 0.1-6um    3800");
                cnt.add("d51                   \\# Cryst_H2O Ice INDX REF 120K intrp up uv6  3800");
                cnt.add("d62    f              \\# Cryst_H2O Ice ABS COEF 120K intrpuv6 ed1  3800");
                cnt.add(String.format("d40   %10.7f       \\# Iron (Fe) Palik+Ordal ABS COEF cm^-1 uv6  3800", values[iv].current));
                cnt.add(String.format("d363  %10.7f       \\# D-O stretch inH2o 4.137efftv ABS COEF e3  3800", values[iv+1].current));
                cnt.add(String.format("%10.7f  %10.7f  1  \\# grain size in cm, abundance, density", values[iv+2].current, values[iv+3].current));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv].titles, values[iv].start, values[iv].end));
                cnt.add(String.format("\\#mixture    %s  %10.7f  %10.7f", values[iv+1].titles, values[iv+1].start, values[iv+1].end));
                cnt.add(String.format("\\#grainsize  %s  %10.7f  %10.7f", values[iv+2].titles, values[iv+2].start, values[iv+2].end));
                cnt.add(String.format("\\#abundance  %s  %10.7f  %10.7f", values[iv+3].titles, values[iv+3].start, values[iv+3].end));
                cnt.add("\\##################");

                cnt.add("\\# End Radtran Mixtures");
//End Specpr Commands 

                cnt.add("20 45 45              \\# Incidence, Emmission, Phase Angle");
                cnt.add("y                     \\# yes, normalized output to Lambert Reflector = 1.0");
                cnt.add("                      \\# s value");
                cnt.add("                      \\# wavelength dependent scattering");
                cnt.add("                      \\# no nf");
                cnt.add("0 \\# 2               \\# FUTURE Areal Mixture components, IF NEEDED");
                cnt.add("\\# d1306   0.00001   \\# FUTURE, NEED 1023ch: 6C.01nFe.02Fe.917HemnFe.04nHnFe.H2O+CO2    352");
                cnt.add("\\# d1312   0.00001   \\# FUTURE, NEED 1023ch: H2Oice+nFe+nHem mol mix+areal.1 10CMPREF   352");
                cnt.add("\\#       1111111111222222222233333333334    \\#  AAAAAAAAAA for use customization");
                cnt.add("\\#34567890123456789012345678901234567890    \\#  nnnnnnn 7-digit number, start at 1 with first model run");

              //NOTE: Ignore - Titles To Be implemented:
                //Users set the model title at the top of this (Strout) user modifiable section
		//cntrlstr[108] = String.format(getTitle() + "     w%8.5f %8.5f %8.5f %8.5f",
		//    plotScaleValues.wminplot, plotScaleValues.wmaxplot, plotScaleValues.plotmin, plotScaleValues.plotmax);

                // cnt.add("radtran 2-layer model AAAAAAAAAA nnnnnnn    \\ title  (nnnnnnn auto set to a unique number");
		modelcount = modelcount +1;
                cnt.add("\\#                                            title  (nnnnnnn auto set to a unique number");
                cnt.add(String.format("radtran 2-layer model=AAAAAAAAAA%07d   w%8.5f %8.5f %8.5f %8.5f",
				modelcount, plotScaleValues.wminplot, plotScaleValues.wmaxplot, plotScaleValues.plotmin, plotScaleValues.plotmax));
                //                              11111111112222222222333333333344444444445555555555666666666677777777778
                //                     12345678901234567890123456789012345678901234567890123456789012345678901234567890
                cnt.add("x                             \\# change to e to auto-write results");
                cnt.add("\\# v                         \\# write computed model to , v = local specpr file");
                cnt.add("\\# v                         \\# write history to v");

                cnt.add("\\# future                               ");
                cnt.add("\\# showbd");


//  end of input, Radtran displays spectrum plot in X-Window
//  waits to exit graphic window with from top of data entry lines 1-3:

              //TO loop and restart new model:  (no file model save; lines 1-3)
//		cntrlstr[0] = "x";  (use e to exit graphics plot mode and save file model)
//		cntrlstr[1] = "e";
//		cntrlstr[2] = "x";

//              OR, to terminate program    (button EXIT)
//
              //System.out.println("EXIT");
//		System.out.println("x");  (use e to exit graphics plot mode and save file model)
//		System.out.println("e");
//		System.out.println("x");
//		System.out.println("EX");
//		System.exit(0);  (Note to close bash window, use:   exit   )
//

		}
	}

        Values[] InitializeValues() {
	//Values[] InitializeValues(Values[] values) {

            int layerArg = 0;
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

            int i = 0;

            int numSliders = 400;
            values = new Values[numSliders];

	// USER CUSTOMIZATION;
	// customize the starting abundances and grain sizes ;

    //Layer 1
    layerArg = 0;  //0 to n-1: 0 = layer 1
        // Layer 1 Thickness (cm)

                //op = "thk"
		titles    = "Layer1 Thickness: ";
		start     = 0.0000000;   // iron (Fe) grain size in cm ;
		end       = 0.1000000;
		current   = 0.0100000;
		materialType = "thickness";
                i = 0;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 1

                //op = "s";
		titles    = "L1 M1 Fe_ gs: ";
		start     = 0.0000001;
		end       = 0.0001000;
		current   = 0.0000152;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles    = "L1 M1 Fe_ ab: ";
		start     = 0.0000000;
		end       = 0.0100000;
		current   = 0.0022574;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 2

                //op = "s";
		titles   = "<html><hr><font color='#4444ff'>L1 M2 thol gs: </font></html>";
		start    = 0.00000010;
		end      = 0.00010000;
		current  = 0.00009300;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles   = "<html><font color='#4444ff'>L1 M2 thol ab: </font></html>";
		start    = 0.00000000;
		end      = 0.05000000;
		current  = 0.00400390;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 3

                //op = "f 2";
		titles   = "<html><hr><font color='#cc3333'>L1 M3 CO2 ab: </font></html>";
		start    = 0.00000000;
		end      = 0.01000000;
		current  = 0.00000000;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles   = "L1 M3 nhm gs: ";
		start    = 0.00000010;
		end      = 0.00500000;
		current  = 0.00490000;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles   = "L1 M3 nHm ab: ";
		start    = 0.00000000;
		end      = 0.99920000;
		current  = 0.07035580;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 4

                //op = "f 2";
		titles   = "<html><hr><font color='#cc3333'>L1 M4 CO2 ab: </font></html>";
		start    = 0.00000000;
		end      = 0.00300000;
		current  = 0.00000000;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles   = "L1 M4 nhm gs: ";
		start    = 0.00000010;
		end      = 0.01100000;
		current  = 0.01000000;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles   = "L1 M4 nHm ab: ";
		start    = 0.00000000;
		end      = 0.70100000;
		current  = 0.07161060;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 5

                //op = "b 3";
		titles  = "<html><hr><font color='#cc3333'>L1 M5 Fe_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0020000;
		current = 0.0000000;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>L1 M5 D-O mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L1 M5 H2O gs: ";
		start   = 0.0000001;
		end     = 0.0900000;
		current = 0.0746000;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L1 M5 H2O ab: ";
		start   = 0.0000000;
		end     = 0.9999900;
		current = 0.1146723;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 6

		titles  = "<html><hr><font color='#cc3333'>L1 M6 Fe_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0080000;
		current = 0.0000000;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>L1 M6 D-O mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L1 M6 H2O gs: ";
		start   = 0.0000001;
		end     = 0.0400000;
		current = 0.0248000;
		materialType = ("grainsize");
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L1 M6 H2O ab: ";
		start   = 0.0000000;
		end     = 0.3000000;
		current = 0.2887614;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 7

		titles  = "<html><hr><font color='#cc3333'>L1 M7 Fe_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0000440;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>L1 M7 D-O mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L1 M7 H2O gs: ";
		start   = 0.0000001;
		end     = 0.0200000;
		current = 0.0055601;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L1 M7 H2O ab: ";
		start   = 0.0000000;
		end     = 0.6000000;
		current = 0.2617969;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 8

		titles  = "<html><hr><font color='#cc3333'>L1 M8 Fe_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001560;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>L1 M8 D-O mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L1 M8 H2O gs: ";
		start   = 0.0000001;
		end     = 0.0020000;
		current = 0.0013281;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L1 M8 H2O ab: ";
		start   = 0.0000000;
		end     = 0.6000000;
		current = 0.1865417;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

    //Layer 2
    layerArg = 1;  //0 to n-1: 1 = layer 2
	// Material 1

                //op = "s";
		titles    = "<html><hr><hr><hr><font color='#4444ff'>L2 M1 Fe_ gs: </font></html>";
		start     = 0.0000001;
		end       = 0.0001000;
		current   = 0.0000152;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles    = "<html><font color='#4444ff'>L2 M1 Fe_ ab: </font></html>";
		start     = 0.0000000;
		end       = 0.0100000;
		current   = 0.0022574;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 2

                //op = "s";
		titles   = "<html><hr><font color='#4444ff'>L2 M2 thol gs: </font></html>";
		start    = 0.00000010;
		end      = 0.00010000;
		current  = 0.00009300;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles   = "<html><font color='#4444ff'>L2 M2 thol ab: </font></html>";
		start    = 0.00000000;
		end      = 0.05000000;
		current  = 0.00400390;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 3

                //op = "f 2";
		titles   = "<html><hr><font color='#cc3333'>L2 M3 CO2 mx: </font></html>";
		start    = 0.00000000;
		end      = 0.01000000;
		current  = 0.00000000;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles   = "L2 M3 nHm gs: ";
		start    = 0.00000010;
		end      = 0.00500000;
		current  = 0.00490000;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles   = "L2 M3 nHm ab: ";
		start    = 0.00000000;
		end      = 0.99920000;
		current  = 0.07035580;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 4

                //op = "f 2";
		titles   = "<html><hr><font color='#cc3333'>L2 M4 CO2 mx: </font></html>";
		start    = 0.00000000;
		end      = 0.00300000;
		current  = 0.00000000;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles   = "L2 M4 nHm gs: ";
		start    = 0.00000010;
		end      = 0.01100000;
		current  = 0.01000000;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles   = "L2 M4 nHm ab: ";
		start    = 0.00000000;
		end      = 0.70100000;
		current  = 0.07161060;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 5

                //op = "b 3";
		titles  = "<html><hr><font color='#cc3333'>L2 M5 Fe_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0020000;
		current = 0.0000000;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>L2 M5 D-O mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L2 M5 H2O gs: ";
		start   = 0.0000001;
		end     = 0.0900000;
		current = 0.0746000;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L2 M5 H2O ab: ";
		start   = 0.0000000;
		end     = 0.9999900;
		current = 0.1146723;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 6

		titles  = "<html><hr><font color='#cc3333'>L2 M6 Fe_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0080000;
		current = 0.0000000;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>L2 M6 D-O mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L2 M6 H2O gs: ";
		start   = 0.0000001;
		end     = 0.0400000;
		current = 0.0248000;
		materialType = ("grainsize");
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L2 M6 H2O ab: ";
		start   = 0.0000000;
		end     = 0.3000000;
		current = 0.2887614;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 7

		titles  = "<html><hr><font color='#cc3333'>L2 M7 Fe_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0000440;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>L2 M7 D-O mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L2 M7 H2O gs: ";
		start   = 0.0000001;
		end     = 0.0200000;
		current = 0.0055601;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L2 M7 H2O ab: ";
		start   = 0.0000000;
		end     = 0.6000000;
		current = 0.2617969;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

	// Material 8

		titles  = "<html><hr><font color='#cc3333'>L2 M8 Fe_ mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001560;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "<html><font color='#33cc33'>L2 M8 D-O mx: </font></html>";
		start   = 0.0000000;
		end     = 0.0050000;
		current = 0.0001800;
		materialType = "mixture";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L2 M8 H2O gs: ";
		start   = 0.0000001;
		end     = 0.0020000;
		current = 0.0013281;
		materialType = "grainsize";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

		titles  = "L2 M8 H2O ab: ";
		start   = 0.0000000;
		end     = 0.6000000;
		current = 0.1865417;
		materialType = "abundance";
                i = i + 1;
		values[i] = new Values(layerArg, titles, start, end, current, materialType);

              //trim array size
              Values[] tmpValues;
                int count = 0;
                for (int ii = 0; ii < values.length; ii++) {
                    if(values[ii] == null) {
                        count = ii;
                        break;
                    }
                }
                if (count > 0) {
                    tmpValues = new Values[count];
                    for (int ii = 0; ii < tmpValues.length; ii++) {
                        tmpValues[ii] = values[ii];
                    }
                } else {
                    tmpValues = values;
                }

                return tmpValues;
        }

        public String getRadGUITitle() {
            return title;
        }

        public void setRadGUITitle(String title) {
            this.title = title;
            //= "Radtran java 10-component gui";
        }

        public String getSPDwrite() {
            return spdWrite;
        }

        public void setSPDwrite(String spdWrite){
            this.spdWrite = spdWrite;
            //= "w";
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
