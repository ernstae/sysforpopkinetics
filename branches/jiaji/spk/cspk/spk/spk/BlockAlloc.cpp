/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
     /*
     ===============================================================================
     $begin BlockAlloc$$
     $path%example%.cc%$$
     $path%.%.h%$$
     $path%.%.cc%$$
     $spell
          Alloc
          info
          ptr
          sizeof
          malloc
		  bool
		  stdio
		  cpp
		  printf
     $$

     $cindex fast block memory allocate$$
     $index BlockAlloc$$

     $section A Fast Block Memory Allocator$$

     $table 
     $bold Syntax$$
     $cend $syntax%%ptr% = BlockAlloc(%size%)%$$ $rend
     $cend $syntax%BlockCapacity(%ptr%)%$$       $rend
     $cend $syntax%BlockFree(%ptr%)%$$           $rend
     $cend $syntax%BlockReturn()%$$              $rend

     $tend

     $fend 25$$

     $center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
     The routines 
     $code BlockAlloc$$,
     $code BlockCapacity$$,
     $code BlockFree$$, and
     $code BlockReturn$$
     are intended for
     fast block allocation and freeing of memory.
     They are written in ANSI C but can be compiled as C++ routines.
     In order to improve speed of allocation,
     these routines are not thread safe and usually provide 
     more memory than is requested.
     $syntax/

     /ptr/ = BlockAlloc(/size/)
     /$$
     sets $italic ptr$$ to a block of memory
     that is at least $italic size$$ units long relative to the 
     $code sizeof$$ function.
     If this request cannot be satisfied,
     the value $code NULL$$ is returned.
     (If the block capacity information is ignored,
     the routine $code BlockAlloc$$ can be used exactly the same
     as the ANSI C standard library routine called $code malloc$$.)
     $syntax/

     BlockCapacity(/ptr/)
     /$$
     returns the number of memory units starting at $italic ptr$$
     that are actually available for use 
     (relative the $code sizeof$$ function).
     If the amount of memory required for use
     increases to some value greater than $italic size$$
     but less than $syntax/BlockCapacity(/ptr/)/$$,
     the memory starting at $italic ptr$$ can be used with out
     any further allocation.
     $syntax/

     BlockFree(/ptr/)
     /$$
     informs $code BlockAlloc$$ that the memory corresponding
     to $italic ptr$$ will no longer be referenced and should be returned to the 
     pool of memory available to $code BlockAlloc$$.
     The return value of $code BlockFree$$ is an integer equal to the number of
     $code BlockAlloc$$ return values that have not yet been freed.
     The argument $italic ptr$$ must either be equal to 
     $code NULL$$ or a value previously 
     returned by $code BlockAlloc$$. 
     In order to conserve memory, it is important that $code BlockFree$$
     be called when a value returned by $code BlockAlloc$$
     is no longer needed.
     (If the return value of $code BlockFree$$ is ignored,
     this routine can be used exactly the same
     as the ANSI C standard library routine called $code free$$.)
     $syntax/

     BlockReturn()
     /$$
     The memory return state is either true (one) or false (zero).
     The initial memory return state is false.
     The state changes after each call to $code BlockReturn$$ and
     the return value of $code BlockReturn$$ is equal to the 
     state after the call. 
     If it is true, all memory that has been freed by $code BlockFree$$,
     or that is subsequently freed by $code BlockFree$$,
     is returned to the system.
     Memory allocation with $code BlockAlloc$$ is slower in this state.
     If the return value of $code BlockReturn$$ is false,
     all memory that is subsequently freed by $code BlockFree$$
     is not returned to the system.
     Memory allocation with $code BlockAlloc$$ is faster in this state.
     If the return value of $code BlockFree(NULL)$$ is zero, 
     and the return value of $code BlockReturn$$ is true,
     all memory freed by $code BlockFree$$ has been returned to the system. 
     If you are using a utility that is checking for memory leaks, 
     you should make sure this is the case when exiting your program.


     $head Types in Syntax$$
     $table
     $syntax%%size%$$                 $cend $syntax%int%$$           $rend
     $syntax%%ptr%$$                  $cend $syntax%void *%$$        $rend
     $syntax%BlockAlloc(%size%)%$$    $cend $syntax%void *%$$        $rend
     $syntax%BlockCapacity(%ptr%)%$$  $cend $syntax%int%$$           $rend
     $syntax%BlockFree(%ptr%)%$$      $cend $syntax%int%$$           $rend
     $syntax%BlockReturn()%$$         $cend $syntax%int%$$ 
     $tend

     $head Debugging$$
     The syntax $code BlockFree(NULL)$$
     can be used to check for memory leaks
     (it should return zero after all the allocated memory has been freed).
     $pre

     $$
     The memory directly before and after 
     each allocated block is checked to see that it has not been modified.
     If a modification is detected,
     an error message is printed and the program will terminate with
     an $code assert$$.
     Other errors, both internal and external to $code BlockAlloc$$
     are checked for.
     If the preprocessor macro $code NDEBUG$$ is defined
     when $code BlockAlloc.cpp$$ is compiled,
     none of this error checking is done and the memory allocator will be faster.
     (The preprocessor $code NDEBUG$$ macro inhibits the action inside 
     ANSI C asserts.)


     $head Example$$
     The program below prints $code Ok$$ if the test succeeds and
     $code Error$$ if the test fails:

     $codep

     # include <stdio.h>
     # include "BlockAlloc.h"

     static bool BlockAlloc_Ok()
     {    
          // number of integers required
          int size = 2;

          // number of blocks initially in use
          int initial = BlockFree(NULL);

          // memory block with space for the integers
          int *i = (int *) BlockAlloc(size * sizeof(int));

          // check the increase in number of block in use
          if( BlockFree(NULL) != initial + 1 )
               return false;

          // write something in that memory
          int capacity = BlockCapacity(i) / sizeof(int);
          if( capacity < size )
               return false;
          int j;
          for(j = 0; j < capacity; j++)
               i[j] = j;

          // check that the memory usage is ok
          for(j = 0; j < capacity; j++)
               if( i[j] != j )
                    return false;

          // free the memory block because it will not be referenced again
          // in addition, check that the in use counter is correct
          if( BlockFree(i) != initial )
               return false;

          // test ok
          return true;
     }

     int main()
     {    bool ok;

          // check initial number of blocks in use
          ok = BlockFree(NULL) == 0;

          // run the test
          if( ok )
               ok = BlockAlloc_Ok();

          // check for a memory leak
          if( ok )
               ok = BlockFree(NULL) == 0;

          // return memory to the operating system
          BlockReturn();
     
          // report result
          if( ok )
               printf("Ok\n");
          else printf("Error\n");
		  return 0;
     }

     $$
     $end
     ===============================================================================
     */
#include <iostream>
     # include <stdlib.h>
     # include <assert.h>
     # include <stdio.h>
     # include "SpkException.h"

     # include "BlockAlloc.h"

     // a number greater than the positive int that is a power of 2
               // 
               // [ Note added by Sachiko, 09/27/2002 ]
               // These assert() statements within the following xxxAssert() macros
               // do not execute in release versions.
               // 
     # define BoundPowerTwo 100

     # ifdef NDEBUG
     # define ExternalAssert(exp,text) \
          if( !(exp) ) \
          throw SpkException( SpkError(SpkError::SPK_UNKNOWN_ERR, "Error in usage of BlockAlloc:\n", __LINE__, __FILE__ ) );

     # define InternalAssert(exp) \
          if( !(exp) ) \
          throw SpkException( SpkError(SpkError::SPK_UNKNOWN_ERR, "Error internal to BlockAlloc:\n", __LINE__, __FILE__ ) );
     # else
     # define ExternalAssert(exp,text) \
          if( ! (exp) ) \
          throw SpkException( SpkError(SpkError::SPK_UNKNOWN_ERR, "Error in usage of BlockAlloc:\n", __LINE__, __FILE__ ) );
     # define InternalAssert(exp) \
          if( ! (exp) ) \
          throw SpkException( SpkError(SpkError::SPK_UNKNOWN_ERR, "Error internal to BlockAlloc:\n", __LINE__, __FILE__ ) );
     # endif

     typedef struct info {

          // index corresponding to capacity of this block
          int index;

          // pointer to memory that BlockAlloc user will recieve
          void *user;

          // pointer to next block
          info *next;

     } Info;


     // vector of lists of available memory blocks
     static Info *MemoryPool[BoundPowerTwo];
     static int    MemoryCapacity[BoundPowerTwo];

     // maximum capacity of a memory block
     static int MaxCapacity     = 0;

     // minumum capacity of a memory block
     static int MinCapacity     = 1;

     // number of allocations returned by BlockAlloc but not freed by BlockFree
     static int NumberInUse = 0;

     // memory return state
     static int MemoryReturnState = 0;

     // Initialize MemoryPool 
     static void Initialize()
     {    int index;
          int capacity;

          InternalAssert( sizeof(unsigned char) == 1 );
          InternalAssert( MaxCapacity  == 0 );
          InternalAssert( MinCapacity  == 1 );

          MinCapacity = 64;
          while( MinCapacity < sizeof(Info) )
               MinCapacity *= 2;

          // initialize each list as empty
          capacity  = MinCapacity;
          index     = 0;
          while(2 * capacity > capacity)
          {    InternalAssert( index < BoundPowerTwo );
     
               MemoryPool[index]     = NULL;
               MemoryCapacity[index] = capacity;

               capacity *= 2;
               index++;
          }
          MaxCapacity = capacity;
     }


     void *BlockAlloc(const int size)
     {    int index;
          int capacity;
          unsigned char *ch;
          Info        *info;

          // check for initialization
          if( MaxCapacity == 0 )
               Initialize();

          // some internal checks
          InternalAssert( MinCapacity >= sizeof(Info) );
          InternalAssert( MaxCapacity >= MinCapacity );

          // make sure memory request is not to large
          ExternalAssert(
               size <= MaxCapacity,
               "size is to large in call to BlockAlloc"
          );

          // determine smallest MemoryPool list corresponding to this capacity
          capacity   = MinCapacity;
          index  = 0;
          while( size > capacity )
          {    InternalAssert( 2 * capacity > capacity );
               capacity *= 2;
               index++;
          }
          InternalAssert( size <= capacity );

          // list of blocks corresponding to this capacity
          if( MemoryPool[index] == NULL ) 
          {    // no blocks left in this list
               info = (Info *) malloc(2 * MinCapacity + capacity);
               
               //
               // [ Comment by Sachiko, 09/26/2002 ]
               // If the return value of malloc() is null,
               // it means there's no enough memory for the requested space.
               //
               // When the size requested were zero, it still returns a
               // valid pointer, not a null.
               //
               if( info == NULL )
                    return NULL;

               MemoryPool[index] = info;
               info->index       = index;
               ch                = ((unsigned char *) info) + MinCapacity;
               info->user        = (void *) ch;
               info->next        = NULL;
          }
          InternalAssert( MemoryPool[index] != NULL );

          // get the first element of the list
          info  = MemoryPool[index];
          InternalAssert( MemoryCapacity[info->index] == capacity );

          // remove it from the available pool of memory
          MemoryPool[index] = info->next;

          // increment the in use counter
          NumberInUse++;

     # ifndef NDEBUG
          // use this information to make sure block is the same upon return
          // and user has not over written the end or beginning of the block
          ch       = ((unsigned char *) info) + MinCapacity;
          Info *b  = (Info *) (ch + capacity);
          b->index = - info->index;
          b->user  = info->user;
          b->next  = info->next;
     # endif

          // return it
          return info->user;
     }

     int BlockCapacity(void *ptr)
     {    unsigned char *ch;
          Info        *info;
          if( ptr == NULL )
               return NumberInUse;

          ch  = (unsigned char *) ptr;
          info = (Info *) (ch - MinCapacity);

          return MemoryCapacity[info->index];
     }

     int BlockFree(void *ptr)
     {    
          unsigned char *ch;
          Info        *info;
     
          if( ptr == NULL )
               return NumberInUse;

          ch  = (unsigned char *) ptr;
          info = (Info *)(ch - MinCapacity);

     # ifndef NDEBUG
          Info *b = (Info *) (ch + MemoryCapacity[info->index]);
          ExternalAssert( 
               (b->index == - info->index) &&
               (b->user == info->user) &&
               (b->next == info->next),
               "Unauthorized memory near ptr has been overwritten"
          );
     # endif

          InternalAssert( MaxCapacity != 0 );

          if( MemoryReturnState )
               free(info);
          else
          {
               info->next              = MemoryPool[info->index];
               MemoryPool[info->index] = info;
          }

          // decrement the in use counter
          NumberInUse--;

          return NumberInUse;
     }

     int BlockReturn(void)
     {    int   capacity;
          int   index;
          Info *list;
          Info *next;

          MemoryReturnState = ! MemoryReturnState;
          if( ! MemoryReturnState )
               return MemoryReturnState;

          capacity  = MinCapacity;
          index     = 0;
          while(2 * capacity > capacity)
          {    InternalAssert( index < BoundPowerTwo );
     
               list = MemoryPool[index];
               while( list != NULL )
               {    next = list->next;
                    free(list);
                    list = next;
               }
               MemoryPool[index] = NULL;

               capacity *= 2;
               index++;
          }
          return MemoryReturnState;
     }


