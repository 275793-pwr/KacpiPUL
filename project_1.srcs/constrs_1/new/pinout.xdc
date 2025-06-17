set_property -dict {PACKAGE_PIN E3 IOSTANDARD LVCMOS33} [get_ports Clock100MHz]
set_property -dict {PACKAGE_PIN A8 IOSTANDARD LVCMOS33} [get_ports {SW[0]}]
set_property -dict {PACKAGE_PIN C11 IOSTANDARD LVCMOS33} [get_ports {SW[1]}]
set_property -dict {PACKAGE_PIN C10 IOSTANDARD LVCMOS33} [get_ports {SW[2]}]
set_property -dict {PACKAGE_PIN A10 IOSTANDARD LVCMOS33} [get_ports {SW[3]}]
set_property -dict {PACKAGE_PIN V15 IOSTANDARD LVCMOS33} [get_ports LCD_RS]
set_property -dict {PACKAGE_PIN U16 IOSTANDARD LVCMOS33} [get_ports LCD_E]
## General XDC file for ARTY & PUL Shield v1.1 2020
## Based on XDC file provided by Digilent

## Clock signal
create_clock -period 10.000 -name sys_clk_pin -waveform {0.000 5.000} -add [get_ports Clock100MHz]

## LEDs

## RGB LEDs
#set_property -dict { PACKAGE_PIN E1    IOSTANDARD LVCMOS33 } [get_ports { LED0_B }]; #IO_L18N_T2_35 Sch=LED0_b
#set_property -dict { PACKAGE_PIN F6    IOSTANDARD LVCMOS33 } [get_ports { LED0_G }]; #IO_L19N_T3_VREF_35 Sch=LED0_g
#set_property -dict { PACKAGE_PIN G6    IOSTANDARD LVCMOS33 } [get_ports { LED0_R }]; #IO_L19P_T3_35 Sch=LED0_r
#set_property -dict { PACKAGE_PIN G4    IOSTANDARD LVCMOS33 } [get_ports { LED1_B }]; #IO_L20P_T3_35 Sch=LED1_b
#set_property -dict { PACKAGE_PIN J4    IOSTANDARD LVCMOS33 } [get_ports { LED1_G }]; #IO_L21P_T3_DQS_35 Sch=LED1_g
#set_property -dict { PACKAGE_PIN G3    IOSTANDARD LVCMOS33 } [get_ports { LED1_R }]; #IO_L20N_T3_35 Sch=LED1_r
#set_property -dict { PACKAGE_PIN H4    IOSTANDARD LVCMOS33 } [get_ports { LED2_B }]; #IO_L21N_T3_DQS_35 Sch=LED2_b
#set_property -dict { PACKAGE_PIN J2    IOSTANDARD LVCMOS33 } [get_ports { LED2_G }]; #IO_L22N_T3_35 Sch=LED2_g
#set_property -dict { PACKAGE_PIN J3    IOSTANDARD LVCMOS33 } [get_ports { LED2_R }]; #IO_L22P_T3_35 Sch=LED2_r
#set_property -dict { PACKAGE_PIN K2    IOSTANDARD LVCMOS33 } [get_ports { LED3_B }]; #IO_L23P_T3_35 Sch=LED3_b
#set_property -dict { PACKAGE_PIN H6    IOSTANDARD LVCMOS33 } [get_ports { LED3_G }]; #IO_L24P_T3_35 Sch=LED3_g
#set_property -dict { PACKAGE_PIN K1    IOSTANDARD LVCMOS33 } [get_ports { LED3_R }]; #IO_L23N_T3_35 Sch=LED3_r

## Switches

## Buttons

## UART
#set_property -dict { PACKAGE_PIN D10   IOSTANDARD LVCMOS33 } [get_ports { UART_TX }]; #IO_L19N_T3_VREF_16 Sch=uart_rxd_out
#set_property -dict { PACKAGE_PIN A9    IOSTANDARD LVCMOS33 } [get_ports { UART_RX }]; #IO_L14N_T2_SRCC_16 Sch=uart_txd_in

## VGA
#set_property -dict { PACKAGE_PIN F5    IOSTANDARD LVCMOS33 } [get_ports { VGA_B }]; #IO_0_35           	Sch=ck_a[0]		ChipKit pin=A0
#set_property -dict { PACKAGE_PIN D8    IOSTANDARD LVCMOS33 } [get_ports { VGA_G }]; #IO_L4P_T0_35      	Sch=ck_a[1]		ChipKit pin=A1
#set_property -dict { PACKAGE_PIN C7    IOSTANDARD LVCMOS33 } [get_ports { VGA_R }]; #IO_L4N_T0_35      	Sch=ck_a[2]		ChipKit pin=A2
#set_property -dict { PACKAGE_PIN E7    IOSTANDARD LVCMOS33 } [get_ports { VGA_HS }]; #IO_L6P_T0_35      	Sch=ck_a[3]		ChipKit pin=A3
#set_property -dict { PACKAGE_PIN H1    IOSTANDARD LVCMOS33 } [get_ports { VGA_VS }]; #IO_L17P_T2_35 Sch=ck_mosi

## Rotary encoder OR PS/2 port
#set_property -dict { PACKAGE_PIN D7    IOSTANDARD LVCMOS33 } [get_ports { Encoder_A }]; #IO_L6N_T0_VREF_35 	Sch=ck_a[4]		ChipKit pin=A4
#set_property -dict { PACKAGE_PIN D5    IOSTANDARD LVCMOS33 } [get_ports { Encoder_B }]; #IO_L11P_T1_SRCC_35	Sch=ck_a[5]		ChipKit pin=A5
#set_property -dict { PACKAGE_PIN D7    IOSTANDARD LVCMOS33 } [get_ports { PS2_Data }]; #IO_L6N_T0_VREF_35 	Sch=ck_a[4]		ChipKit pin=A4
#set_property -dict { PACKAGE_PIN D5    IOSTANDARD LVCMOS33 } [get_ports { PS2_Clock }]; #IO_L11P_T1_SRCC_35	Sch=ck_a[5]		ChipKit pin=A5

## 7 Segment LED Display

## LCD Display

### Audio I2S Codec
#set_property -dict { PACKAGE_PIN R10   IOSTANDARD LVCMOS33 } [get_ports { I2S_DIN }]; #IO_25_14 		 			Sch=ck_io[29]
#set_property -dict { PACKAGE_PIN R12   IOSTANDARD LVCMOS33 } [get_ports { I2S_DOUT  }]; #IO_L5P_T0_D06_14             Sch=ck_io[4]
#set_property -dict { PACKAGE_PIN T14   IOSTANDARD LVCMOS33 } [get_ports { I2S_LRCLK  }]; #IO_L14P_T2_SRCC_14           Sch=ck_io[5]
#set_property -dict { PACKAGE_PIN R11   IOSTANDARD LVCMOS33 } [get_ports { I2S_SCLK }]; #IO_0_14  		 			Sch=ck_io[30]
#set_property -dict { PACKAGE_PIN R13   IOSTANDARD LVCMOS33 } [get_ports { SYS_MCLK }]; #IO_L5N_T0_D07_14 			Sch=ck_io[31]

## Use 595 OR (LCD and I2S)
## 74HC595
#set_property -dict { PACKAGE_PIN U11   IOSTANDARD LVCMOS33 } [get_ports { 595_DS }]; #IO_L19N_T3_A09_D25_VREF_14 	Sch=ck_io[26]
#set_property -dict { PACKAGE_PIN V16   IOSTANDARD LVCMOS33 } [get_ports { 595_nMR }]; #IO_L16N_T2_A15_D31_14 		Sch=ck_io[27]
#set_property -dict { PACKAGE_PIN V15   IOSTANDARD LVCMOS33 } [get_ports { 595_Q0 }]; #IO_L16P_T2_CSI_B_14          Sch=ck_io[0]
#set_property -dict { PACKAGE_PIN M13   IOSTANDARD LVCMOS33 } [get_ports { 595_Q1 }]; #IO_L6N_T0_D08_VREF_14 		Sch=ck_io[28]
#set_property -dict { PACKAGE_PIN T11   IOSTANDARD LVCMOS33 } [get_ports { 595_Q2 }]; #IO_L19P_T3_A10_D26_14        Sch=ck_io[3]
#set_property -dict { PACKAGE_PIN R10   IOSTANDARD LVCMOS33 } [get_ports { 595_Q3 }]; #IO_25_14 		 			Sch=ck_io[29]
#set_property -dict { PACKAGE_PIN R12   IOSTANDARD LVCMOS33 } [get_ports { 595_Q4 }]; #IO_L5P_T0_D06_14             Sch=ck_io[4]
#set_property -dict { PACKAGE_PIN R11   IOSTANDARD LVCMOS33 } [get_ports { 595_Q5 }]; #IO_0_14  		 			Sch=ck_io[30]
#set_property -dict { PACKAGE_PIN T14   IOSTANDARD LVCMOS33 } [get_ports { 595_Q6 }]; #IO_L14P_T2_SRCC_14           Sch=ck_io[5]
#set_property -dict { PACKAGE_PIN P14   IOSTANDARD LVCMOS33 } [get_ports { 595_Q7S  }]; #IO_L8N_T1_D12_14             Sch=ck_io[2]
#set_property -dict { PACKAGE_PIN U16   IOSTANDARD LVCMOS33 } [get_ports { 595_STCP_SHCP  }]; #IO_L18P_T2_A12_D28_14        Sch=ck_io[1]

## Heater and Buzzer
#set_property -dict { PACKAGE_PIN U17   IOSTANDARD LVCMOS33 } [get_ports { PWM }]; #IO_L17P_T2_A14_D30_14 		Sch=ck_io[37]
#set_property -dict { PACKAGE_PIN M17   IOSTANDARD LVCMOS33 } [get_ports { Buzzer }]; #IO_L10N_T1_D15_14 Sch=ck_ioa

### DAC
#set_property -dict { PACKAGE_PIN P18   IOSTANDARD LVCMOS33 } [get_ports { DAC_SCLK }]; #IO_L9N_T1_DQS_D13_14 		Sch=ck_io[40]
#set_property -dict { PACKAGE_PIN P17   IOSTANDARD LVCMOS33 } [get_ports { DAC_CS   }]; #IO_L12P_T1_MRCC_14           Sch=ck_io[13]
#set_property -dict { PACKAGE_PIN N17   IOSTANDARD LVCMOS33 } [get_ports { DAC_DIN  }]; #IO_L9P_T1_DQS_14 			Sch=ck_io[41]
#set_property -dict { PACKAGE_PIN R18   IOSTANDARD LVCMOS33 } [get_ports { DAC_LDAC }]; #IO_L7P_T1_D09_14 			Sch=ck_io[39]

### ADC
#set_property -dict { PACKAGE_PIN U18   IOSTANDARD LVCMOS33 } [get_ports { ADC_CLK  }]; #IO_L17N_T2_A13_D29_14        Sch=ck_io[11]
#set_property -dict { PACKAGE_PIN R17   IOSTANDARD LVCMOS33 } [get_ports { ADC_CS   }]; #IO_L12N_T1_MRCC_14           Sch=ck_io[12]
#set_property -dict { PACKAGE_PIN T18   IOSTANDARD LVCMOS33 } [get_ports { ADC_DOUT }]; #IO_L7N_T1_D10_14 			Sch=ck_io[38]

### I2C and additional Pull-Ups
#set_property -dict { PACKAGE_PIN L18   IOSTANDARD LVCMOS33 } [get_ports { I2C_SCL  }]; #IO_L4P_T0_D04_14 Sch=ck_scl
#set_property -dict { PACKAGE_PIN M18   IOSTANDARD LVCMOS33 } [get_ports { I2C_SDA  }]; #IO_L4N_T0_D05_14 Sch=ck_sda
#set_property -dict { PACKAGE_PIN A14   IOSTANDARD LVCMOS33 } [get_ports { I2C_SCL_PU }]; #IO_L9N_T1_DQS_AD3N_15 Sch=scl_pup
#set_property -dict { PACKAGE_PIN A13   IOSTANDARD LVCMOS33 } [get_ports { I2C_SDA_PU }]; #IO_L9P_T1_DQS_AD3P_15 Sch=sda_pup


## Pmod Header JA
#set_property -dict { PACKAGE_PIN G13   IOSTANDARD LVCMOS33 } [get_ports { ja[0] }]; #IO_0_15 Sch=ja[1]
#set_property -dict { PACKAGE_PIN B11   IOSTANDARD LVCMOS33 } [get_ports { ja[1] }]; #IO_L4P_T0_15 Sch=ja[2]
#set_property -dict { PACKAGE_PIN A11   IOSTANDARD LVCMOS33 } [get_ports { ja[2] }]; #IO_L4N_T0_15 Sch=ja[3]
#set_property -dict { PACKAGE_PIN D12   IOSTANDARD LVCMOS33 } [get_ports { ja[3] }]; #IO_L6P_T0_15 Sch=ja[4]
#set_property -dict { PACKAGE_PIN D13   IOSTANDARD LVCMOS33 } [get_ports { ja[4] }]; #IO_L6N_T0_VREF_15 Sch=ja[7]
#set_property -dict { PACKAGE_PIN B18   IOSTANDARD LVCMOS33 } [get_ports { ja[5] }]; #IO_L10P_T1_AD11P_15 Sch=ja[8]
#set_property -dict { PACKAGE_PIN A18   IOSTANDARD LVCMOS33 } [get_ports { ja[6] }]; #IO_L10N_T1_AD11N_15 Sch=ja[9]
#set_property -dict { PACKAGE_PIN K16   IOSTANDARD LVCMOS33 } [get_ports { ja[7] }]; #IO_25_15 Sch=ja[10]

## Pmod Header JB
#set_property -dict { PACKAGE_PIN E15   IOSTANDARD LVCMOS33 } [get_ports { jb[0] }]; #IO_L11P_T1_SRCC_15 Sch=jb_p[1]
#set_property -dict { PACKAGE_PIN E16   IOSTANDARD LVCMOS33 } [get_ports { jb[1] }]; #IO_L11N_T1_SRCC_15 Sch=jb_n[1]
#set_property -dict { PACKAGE_PIN D15   IOSTANDARD LVCMOS33 } [get_ports { jb[2] }]; #IO_L12P_T1_MRCC_15 Sch=jb_p[2]
#set_property -dict { PACKAGE_PIN C15   IOSTANDARD LVCMOS33 } [get_ports { jb[3] }]; #IO_L12N_T1_MRCC_15 Sch=jb_n[2]
#set_property -dict { PACKAGE_PIN J17   IOSTANDARD LVCMOS33 } [get_ports { jb[4] }]; #IO_L23P_T3_FOE_B_15 Sch=jb_p[3]
#set_property -dict { PACKAGE_PIN J18   IOSTANDARD LVCMOS33 } [get_ports { jb[5] }]; #IO_L23N_T3_FWE_B_15 Sch=jb_n[3]
#set_property -dict { PACKAGE_PIN K15   IOSTANDARD LVCMOS33 } [get_ports { jb[6] }]; #IO_L24P_T3_RS1_15 Sch=jb_p[4]
#set_property -dict { PACKAGE_PIN J15   IOSTANDARD LVCMOS33 } [get_ports { jb[7] }]; #IO_L24N_T3_RS0_15 Sch=jb_n[4]

## Pmod Header JC
#set_property -dict { PACKAGE_PIN U12   IOSTANDARD LVCMOS33 } [get_ports { jc[0] }]; #IO_L20P_T3_A08_D24_14 Sch=jc_p[1]
#set_property -dict { PACKAGE_PIN V12   IOSTANDARD LVCMOS33 } [get_ports { jc[1] }]; #IO_L20N_T3_A07_D23_14 Sch=jc_n[1]
#set_property -dict { PACKAGE_PIN V10   IOSTANDARD LVCMOS33 } [get_ports { jc[2] }]; #IO_L21P_T3_DQS_14 Sch=jc_p[2]
#set_property -dict { PACKAGE_PIN V11   IOSTANDARD LVCMOS33 } [get_ports { jc[3] }]; #IO_L21N_T3_DQS_A06_D22_14 Sch=jc_n[2]
#set_property -dict { PACKAGE_PIN U14   IOSTANDARD LVCMOS33 } [get_ports { jc[4] }]; #IO_L22P_T3_A05_D21_14 Sch=jc_p[3]
#set_property -dict { PACKAGE_PIN V14   IOSTANDARD LVCMOS33 } [get_ports { jc[5] }]; #IO_L22N_T3_A04_D20_14 Sch=jc_n[3]
#set_property -dict { PACKAGE_PIN T13   IOSTANDARD LVCMOS33 } [get_ports { jc[6] }]; #IO_L23P_T3_A03_D19_14 Sch=jc_p[4]
#set_property -dict { PACKAGE_PIN U13   IOSTANDARD LVCMOS33 } [get_ports { jc[7] }]; #IO_L23N_T3_A02_D18_14 Sch=jc_n[4]

## Pmod Header JD
#set_property -dict { PACKAGE_PIN D4    IOSTANDARD LVCMOS33 } [get_ports { jd[0] }]; #IO_L11N_T1_SRCC_35 Sch=jd[1]
#set_property -dict { PACKAGE_PIN D3    IOSTANDARD LVCMOS33 } [get_ports { jd[1] }]; #IO_L12N_T1_MRCC_35 Sch=jd[2]
#set_property -dict { PACKAGE_PIN F4    IOSTANDARD LVCMOS33 } [get_ports { jd[2] }]; #IO_L13P_T2_MRCC_35 Sch=jd[3]
#set_property -dict { PACKAGE_PIN F3    IOSTANDARD LVCMOS33 } [get_ports { jd[3] }]; #IO_L13N_T2_MRCC_35 Sch=jd[4]
#set_property -dict { PACKAGE_PIN E2    IOSTANDARD LVCMOS33 } [get_ports { jd[4] }]; #IO_L14P_T2_SRCC_35 Sch=jd[7]
#set_property -dict { PACKAGE_PIN D2    IOSTANDARD LVCMOS33 } [get_ports { jd[5] }]; #IO_L14N_T2_SRCC_35 Sch=jd[8]
#set_property -dict { PACKAGE_PIN H2    IOSTANDARD LVCMOS33 } [get_ports { jd[6] }]; #IO_L15P_T2_DQS_35 Sch=jd[9]
#set_property -dict { PACKAGE_PIN G2    IOSTANDARD LVCMOS33 } [get_ports { jd[7] }]; #IO_L15N_T2_DQS_35 Sch=jd[10]

## SMSC Ethernet PHY
#set_property -dict { PACKAGE_PIN D17   IOSTANDARD LVCMOS33 } [get_ports { eth_col }]; #IO_L16N_T2_A27_15 Sch=eth_col
#set_property -dict { PACKAGE_PIN G14   IOSTANDARD LVCMOS33 } [get_ports { eth_crs }]; #IO_L15N_T2_DQS_ADV_B_15 Sch=eth_crs
#set_property -dict { PACKAGE_PIN F16   IOSTANDARD LVCMOS33 } [get_ports { eth_mdc }]; #IO_L14N_T2_SRCC_15 Sch=eth_mdc
#set_property -dict { PACKAGE_PIN K13   IOSTANDARD LVCMOS33 } [get_ports { eth_mdio }]; #IO_L17P_T2_A26_15 Sch=eth_mdio
#set_property -dict { PACKAGE_PIN G18   IOSTANDARD LVCMOS33 } [get_ports { eth_ref_clk }]; #IO_L22P_T3_A17_15 Sch=eth_ref_clk
#set_property -dict { PACKAGE_PIN C16   IOSTANDARD LVCMOS33 } [get_ports { eth_rstn }]; #IO_L20P_T3_A20_15 Sch=eth_rstn
#set_property -dict { PACKAGE_PIN F15   IOSTANDARD LVCMOS33 } [get_ports { eth_rx_clk }]; #IO_L14P_T2_SRCC_15 Sch=eth_rx_clk
#set_property -dict { PACKAGE_PIN G16   IOSTANDARD LVCMOS33 } [get_ports { eth_rx_dv }]; #IO_L13N_T2_MRCC_15 Sch=eth_rx_dv
#set_property -dict { PACKAGE_PIN D18   IOSTANDARD LVCMOS33 } [get_ports { eth_rxd[0] }]; #IO_L21N_T3_DQS_A18_15 Sch=eth_rxd[0]
#set_property -dict { PACKAGE_PIN E17   IOSTANDARD LVCMOS33 } [get_ports { eth_rxd[1] }]; #IO_L16P_T2_A28_15 Sch=eth_rxd[1]
#set_property -dict { PACKAGE_PIN E18   IOSTANDARD LVCMOS33 } [get_ports { eth_rxd[2] }]; #IO_L21P_T3_DQS_15 Sch=eth_rxd[2]
#set_property -dict { PACKAGE_PIN G17   IOSTANDARD LVCMOS33 } [get_ports { eth_rxd[3] }]; #IO_L18N_T2_A23_15 Sch=eth_rxd[3]
#set_property -dict { PACKAGE_PIN C17   IOSTANDARD LVCMOS33 } [get_ports { eth_rxerr }]; #IO_L20N_T3_A19_15 Sch=eth_rxerr
#set_property -dict { PACKAGE_PIN H16   IOSTANDARD LVCMOS33 } [get_ports { eth_tx_clk }]; #IO_L13P_T2_MRCC_15 Sch=eth_tx_clk
#set_property -dict { PACKAGE_PIN H15   IOSTANDARD LVCMOS33 } [get_ports { eth_tx_en }]; #IO_L19N_T3_A21_VREF_15 Sch=eth_tx_en
#set_property -dict { PACKAGE_PIN H14   IOSTANDARD LVCMOS33 } [get_ports { eth_txd[0] }]; #IO_L15P_T2_DQS_15 Sch=eth_txd[0]
#set_property -dict { PACKAGE_PIN J14   IOSTANDARD LVCMOS33 } [get_ports { eth_txd[1] }]; #IO_L19P_T3_A22_15 Sch=eth_txd[1]
#set_property -dict { PACKAGE_PIN J13   IOSTANDARD LVCMOS33 } [get_ports { eth_txd[2] }]; #IO_L17N_T2_A25_15 Sch=eth_txd[2]
#set_property -dict { PACKAGE_PIN H17   IOSTANDARD LVCMOS33 } [get_ports { eth_txd[3] }]; #IO_L18P_T2_A24_15 Sch=eth_txd[3]


#set_property IOSTANDARD LVCMOS33 [get_ports {BUTTON[3]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {BUTTON[2]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {BUTTON[1]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {BUTTON[0]}]
#set_property PACKAGE_PIN B8 [get_ports {BUTTON[3]}]
#set_property PACKAGE_PIN B9 [get_ports {BUTTON[2]}]
#set_property PACKAGE_PIN C9 [get_ports {BUTTON[1]}]
#set_property PACKAGE_PIN D9 [get_ports {BUTTON[0]}]

set_property IOSTANDARD LVCMOS33 [get_ports SPI_SS]
set_property IOSTANDARD LVCMOS33 [get_ports SPI_SCK]
set_property PACKAGE_PIN U18 [get_ports SPI_SCK]
set_property PACKAGE_PIN R17 [get_ports SPI_SS]

set_property PACKAGE_PIN R18 [get_ports LDAC]
set_property IOSTANDARD LVCMOS33 [get_ports LDAC]


set_property PACKAGE_PIN V16 [get_ports {LCD_DATA[0]}]
set_property PACKAGE_PIN P14 [get_ports {LCD_DATA[1]}]
set_property PACKAGE_PIN M13 [get_ports {LCD_DATA[2]}]
set_property PACKAGE_PIN T11 [get_ports {LCD_DATA[3]}]
set_property IOSTANDARD LVCMOS33 [get_ports {LCD_DATA[3]}]
set_property IOSTANDARD LVCMOS33 [get_ports {LCD_DATA[2]}]
set_property IOSTANDARD LVCMOS33 [get_ports {LCD_DATA[1]}]
set_property IOSTANDARD LVCMOS33 [get_ports {LCD_DATA[0]}]

set_property IOSTANDARD LVCMOS33 [get_ports {DEBUG[7]}]
set_property IOSTANDARD LVCMOS33 [get_ports {DEBUG[6]}]
set_property IOSTANDARD LVCMOS33 [get_ports {DEBUG[5]}]
set_property IOSTANDARD LVCMOS33 [get_ports {DEBUG[4]}]
set_property IOSTANDARD LVCMOS33 [get_ports {DEBUG[3]}]
set_property IOSTANDARD LVCMOS33 [get_ports {DEBUG[2]}]
set_property IOSTANDARD LVCMOS33 [get_ports {DEBUG[1]}]
set_property IOSTANDARD LVCMOS33 [get_ports {DEBUG[0]}]
set_property PACKAGE_PIN F1 [get_ports {DEBUG[0]}]
set_property PACKAGE_PIN C1 [get_ports {DEBUG[1]}]
set_property PACKAGE_PIN G1 [get_ports {DEBUG[2]}]
set_property PACKAGE_PIN T15 [get_ports {DEBUG[3]}]
set_property PACKAGE_PIN R15 [get_ports {DEBUG[4]}]
set_property PACKAGE_PIN T16 [get_ports {DEBUG[5]}]
set_property PACKAGE_PIN P15 [get_ports {DEBUG[6]}]
set_property PACKAGE_PIN R16 [get_ports {DEBUG[7]}]



set_property PACKAGE_PIN T18 [get_ports SPI_MISO]
set_property IOSTANDARD LVCMOS33 [get_ports SPI_MISO]
