/*
 *  TEST SUITE FOR MB/WC FUNCTIONS IN C LIBRARY
 *
 *	 FILE:	dat_wcspbrk.c
 *
 *	 WCSSTR:  wchar_t *wcspbrk (const wchar_t *ws1, const wchar_t *ws2);
 */


/*
 * NOTE:
 *	 This is not a locale sensitive function.
 *	 So those data in each locale doesn't make sense
 *	 ... (redundant test cases)
 */


TST_WCSPBRK tst_wcspbrk_loc [] = {
  {
    { Twcspbrk, TST_LOC_de },
    {
      { /*input.*/ { { 0x00D1,0x00D2,0x00D3,0x0000 },
		     { 0x00D1,0x0000		   }, },  /* #01 */
	/*expect*/ { 0,0,0,		    0x00D1    },
      },
      { /*input.*/ { { 0x00D1,0x00D2,0x00D3,0x0000 },
		     {	      0x00D2,0x0000	   }, },  /* #02 */
	/*expect*/ { 0,0,0,		    0x00D2    },
      },
      { /*input.*/ { { 0x00D1,0x00D2,0x00D3,0x0000 },
		     {		     0x00D3,0x0000 }, },  /* #03 */
	/*expect*/ { 0,0,0,		    0x00D3    },
      },
      { /*input.*/ { { 0x00D1,0x00D2,0x00D3,0x0000 },
		     { 0x00D1,0x00D2,0x0000	   }, },  /* #04 */
	/*expect*/ { 0,0,0,		    0x00D1    },
      },
      { /*input.*/ { { 0x00D1,0x00D2,0x00D3,0x0000 },
		     {	      0x00D2,0x00D3,0x0000 }, },  /* #05 */
	/*expect*/ { 0,0,0,		    0x00D2    },
      },
      { /*input.*/ { { 0x00D1,0x00D2,0x00D3,0x0000 },
		     { 0x00D1,0x00D2,0x00D3,0x0000 }, },  /* #06 */
	/*expect*/ { 0,0,0,		    0x00D1    },
      },
      { /*input.*/ { { 0x00D1,0x00D2,0x00D3,0x0000 },
		     { 0x00D0,0x00D4,0x00D5,0x0000 }, },  /* #07 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { /*input.*/ { { 0x00D1,0x00D2,0x00D3,0x0000 },
		     { 0x00D5,0x00D0,0x00D4,0x0000 }, },  /* #08 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { /*input.*/ { { 0x00D1,0x00D2,0x00D3,0x0000 },
		     { 0x0000			   }, },  /* #09 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { /*input.*/ { { 0x0000,0x00D2,0x00D3,0x0000 },
		     { 0x00D1,0x0000		   }, },  /* #10 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { /*input.*/ { { 0x0000,0x00D2,0x00D3,0x0000 },
		     { 0x0000			   }, },  /* #11 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { is_last: 1 }
    }
  },
  {
    { Twcspbrk, TST_LOC_enUS },
    {
      { /*input.*/ { { 0x0041,0x0042,0x0043,0x0000 },
		     { 0x0041,0x0000		   }, },  /* #01 */
	/*expect*/ { 0,0,0,		    0x0041    },
      },
      { /*input.*/ { { 0x0041,0x0042,0x0043,0x0000 },
		     {	      0x0042,0x0000	   }, },  /* #02 */
	/*expect*/ { 0,0,0,		    0x0042    },
      },
      { /*input.*/ { { 0x0041,0x0042,0x0043,0x0000 },
		     {		     0x0043,0x0000 }, },  /* #03 */
	/*expect*/ { 0,0,0,		    0x0043    },
      },
      { /*input.*/ { { 0x0041,0x0042,0x0043,0x0000 },
		     { 0x0041,0x0042,0x0000	   }, },  /* #04 */
	/*expect*/ { 0,0,0,		    0x0041    },
      },
      { /*input.*/ { { 0x0041,0x0042,0x0043,0x0000 },
		     {	      0x0042,0x0043,0x0000 }, },  /* #05 */
	/*expect*/ { 0,0,0,		    0x0042    },
      },
      { /*input.*/ { { 0x0041,0x0042,0x0043,0x0000 },
		     { 0x0041,0x0042,0x0043,0x0000 }, },  /* #06 */
	/*expect*/ { 0,0,0,		    0x0041    },
      },
      { /*input.*/ { { 0x0041,0x0042,0x0043,0x0000 },
		     { 0x0040,0x0044,0x0045,0x0000 }, },  /* #07 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { /*input.*/ { { 0x0041,0x0042,0x0043,0x0000 },
		     { 0x0045,0x0040,0x0044,0x0000 }, },  /* #08 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { /*input.*/ { { 0x0041,0x0042,0x0043,0x0000 },
		     { 0x0000			   }, },  /* #09 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { /*input.*/ { { 0x0000,0x0042,0x0043,0x0000 },
		     { 0x0041,0x0000		   }, },  /* #10 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { /*input.*/ { { 0x0000,0x0042,0x0043,0x0000 },
		     { 0x0000			   }, },  /* #11 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { is_last: 1 }
    }
  },
  {
    { Twcspbrk, TST_LOC_eucJP },
    {
      { /*input.*/ { { 0x3041,0x3042,0x3043,0x0000 },
		     { 0x3041,0x0000		   }, },  /* #01 */
	/*expect*/ { 0,0,0,		    0x3041    },
      },
      { /*input.*/ { { 0x3041,0x3042,0x3043,0x0000 },
		     {	      0x3042,0x0000	   }, },  /* #02 */
	/*expect*/ { 0,0,0,		    0x3042    },
      },
      { /*input.*/ { { 0x3041,0x3042,0x3043,0x0000 },
		     {		     0x3043,0x0000 }, },  /* #03 */
	/*expect*/ { 0,0,0,		    0x3043    },
      },
      { /*input.*/ { { 0x3041,0x3042,0x3043,0x0000 },
		     { 0x3041,0x3042,0x0000	   }, },  /* #04 */
	/*expect*/ { 0,0,0,		    0x3041    },
      },
      { /*input.*/ { { 0x3041,0x3042,0x3043,0x0000 },
		     {	      0x3042,0x3043,0x0000 }, },  /* #05 */
	/*expect*/ { 0,0,0,		    0x3042    },
      },
      { /*input.*/ { { 0x3041,0x3042,0x3043,0x0000 },
		     { 0x3041,0x3042,0x3043,0x0000 }, },  /* #06 */
	/*expect*/ { 0,0,0,		    0x3041    },
      },
      { /*input.*/ { { 0x3041,0x3042,0x3043,0x0000 },
		     { 0x3042,0x3043,0x3044,0x0000 }, },  /* #07 */
	/*expect*/ { 0,0,0, 0x3042		      },
      },
      { /*input.*/ { { 0x3041,0x3042,0x3043,0x0000 },
		     { 0x3040,0x3041,0x3042,0x0000 }, },  /* #08 */
	/*expect*/ { 0,0,0, 0x3041		      },
      },
      { /*input.*/ { { 0x3041,0x3042,0x3043,0x0000 },
		     { 0x0000			   }, },  /* #09 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { /*input.*/ { { 0x0000,0x3042,0x3043,0x0000 },
		     { 0x3041,0x0000		   }, },  /* #10 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { /*input.*/ { { 0x0000,0x3042,0x3043,0x0000 },
		     { 0x0000			   }, },  /* #11 */
	/*expect*/ { 0,1,(wchar_t *)NULL, 0x0000    },
      },
      { is_last: 1 }
    }
  },
  {
    { Twcspbrk, TST_LOC_end }
  }
};
