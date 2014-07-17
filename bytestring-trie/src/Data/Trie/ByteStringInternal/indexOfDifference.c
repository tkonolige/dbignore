/* -------------------------------------------------------------
--                                                  ~ 2009.01.07
-- |
-- Module      :  Data.Trie.ByteStringInternal.indexOfDifference
-- Copyright   :  Copyright (c) 2008--2011 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  beta
-- Portability :  portable
--
-- More efficient implementation (in theory) than testing characters
-- byte by byte. However, the gains seem to be minimal and ---more
-- importantly--- there seem to be obscure bugs that only show up
-- in extensive testing (e.g. running TrieFile on /usr/dict). Maybe
-- worth pursuing further, especially if SSE's 126-bit registers
-- or GCC's vectors can be leveraged in a portable way.
------------------------------------------------------------- */

/* Defines certain architecture characteristics.
 * Created by Configure.hs */
#include "indexOfDifference.h"

typedef unsigned char      Word8;
typedef unsigned short     Word16;
typedef unsigned long      Word32;
typedef unsigned long long Word64;

/* Hopefully this makes Nat the most optimal size, or the same as int */
#ifdef __hDataTrie_Nat64__
	typedef Word64 Nat;
#	define MIN_NAT ((Nat) 0x8000000000000000ull)
#elif defined(__hDataTrie_Nat32__)
	typedef Word32 Nat;
#	define MIN_NAT ((Nat) 0x80000000)
#else
	/* No definition. Unknown architecture.
	 * Maybe it'd be worth supporting 16-bit as well...?
	 * or maybe fail back to 8-bit? */
#endif

/* cf also <http://www.monkeyspeak.com/alignment/> */
#define NAT_MISALIGNMENT(p) (((int)(p)) % sizeof(Nat))


#define READ_WORD8(p) (*((Word8*) (p)))
#define READ_NAT(p)   (*((Nat*)   (p)))


/* ---------------------------------------------------------- */
/* Compare up to the first @limit@ bytes of @p1@ against @p2@
 * and return the first index where they differ. */

/* TODO: Consider replacing loops by Duff's Device, or similar
   <http://en.wikipedia.org/wiki/Duff%27s_device> */

int indexOfDifference(const void* p1, const void* p2, const int limit) {
	/* @i@ measures how many bytes are shared,
	 * Thus it's the 0-based index of difference. */
	int i = 0;
	if (limit <= 0) return 0;
	
	
	/* Munge until Nat-aligned (They seem to always be).
	 * Should fail back to this naive version on unknown arch */
	{
		int x1 = NAT_MISALIGNMENT(p1);
		int x2 = NAT_MISALIGNMENT(p2);
		if (x1 != x2) {
			/* FIX: what if they're misaligned differently?
			 * For now we'll use Word8 all the way */
			x1 = limit;
		}
		while (i < x1) {
			if (READ_WORD8(p1+i) == READ_WORD8(p2+i)) {
				i += sizeof(Word8);
			} else {
				return i;
			}
		}
		if (x1 == limit) {
			return limit;
		} else {
			/* Fall through */
		}
	}
	
	
	/* Check one Nat at a time until we find a mismatch */
	Nat diff;
	do {
		diff = READ_NAT(p1+i) ^ READ_NAT(p2+i);
		
		/* If the diff is valid and zero, then increment the loop */
		if (i + sizeof(Nat) <= limit && diff == 0) {
			i += sizeof(Nat);
		} else {
			break;
		}
	} while (i < limit);
	
	
	/* Trim incomplete Nat at the end of the strings */
	if (i + sizeof(Nat) > limit) {
		#ifdef __hDataTrie_isLittleEndian__
			diff &= ((1 << ((limit-i) * 8*sizeof(Word8))) - 1);
		#else
			diff &= MIN_NAT >> ((limit-i) * 8*sizeof(Word8) - 1);
		#endif
		
		if (0 == diff) return limit;
	}
	
	
	/* Found a difference. Do binary search to identify first
	 * byte in Nat which doesn't match. */
	Word32 w32;
	#ifdef __hDataTrie_Nat64__
	#	ifdef __hDataTrie_isLittleEndian__
			const Word32 first32 = (Word32) (diff & 0x00000000FFFFFFFFull);
	#	else
			const Word32 first32 = (Word32)((diff & 0xFFFFFFFF00000000ull) >> 32);
	#	endif
		if (0 == first32) {
			i += 4;
	#		ifdef __hDataTrie_isLittleEndian__
				w32 = (Word32)((diff & 0xFFFFFFFF00000000ull) >> 32);
	#		else
				w32 = (Word32) (diff & 0x00000000FFFFFFFFull);
	#		endif
		} else {
			w32 = first32;
		}
	#elif defined(__hDataTrie_Nat32__)
		w32 = diff;
	#else
		/* WTF? */
	#endif
	
	
	Word16 w16;
	#if defined(__hDataTrie_Nat32__) || defined(__hDataTrie_Nat64__)
	#	ifdef __hDataTrie_isLittleEndian__
			const Word16 first16 = (Word16) (w32 & 0x0000FFFF);
	#	else
			const Word16 first16 = (Word16)((w32 & 0xFFFF0000) >> 16);
	#	endif
		if (0 == first16) {
			i += 2;
	#		ifdef __hDataTrie_isLittleEndian__
				w16 = (Word16)((w32 & 0xFFFF0000) >> 16);
	#		else
				w16 = (Word16) (w32 & 0x0000FFFF);
	#		endif
		} else {
			w16 = first16;
		}
	#else
		/* WTF? */
	#endif
	
	
	Word8 w8;
	#ifdef __hDataTrie_isLittleEndian__
		const Word8 first8 = (Word8) (w16 & 0x00FF);
	#else
		const Word8 first8 = (Word8)((w16 & 0xFF00) >> 8);
	#endif
	if (0 == first8) {
		i += 1;
	}
	
	return i;
}
/* ---------------------------------------------------------- */
/* ----------------------------------------------------- fin. */
