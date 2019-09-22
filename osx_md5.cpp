//
//  osx_md5.cpp
//  ProtocolCompiler
//
//  Created by Rich Neswold on 9/1/16.
//  Copyright © 2016 Fermilab. All rights reserved.
//

#include <inttypes.h>
#define COMMON_DIGEST_FOR_OPENSSL
#import <CommonCrypto/CommonDigest.h>
#include "pc.h"

int32_t md5hash(void const* buf1, size_t const sz1,
                void const* buf2, size_t const sz2)
{
    unsigned char result[MD5_DIGEST_LENGTH];
    MD5_CTX ctx;

    MD5_Init(&ctx);
    MD5_Update(&ctx, buf1, sz1);
    MD5_Update(&ctx, buf2, sz2);
    MD5_Final(result, &ctx);

    return (int32_t)(((uint32_t) result[0] << 24) |
                     ((uint32_t) result[1] << 16) |
                     ((uint32_t) result[2] << 8) |
                     (uint32_t) result[3]);
}
