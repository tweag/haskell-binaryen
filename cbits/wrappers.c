#include <binaryen-c.h>

BinaryenExpressionRef BinaryenConstInt32(BinaryenModuleRef module, int32_t x) { return BinaryenConst(module, BinaryenLiteralInt32(x)); }
BinaryenExpressionRef BinaryenConstInt64(BinaryenModuleRef module, int64_t x) { return BinaryenConst(module, BinaryenLiteralInt64(x)); }
BinaryenExpressionRef BinaryenConstFloat32(BinaryenModuleRef module, float x) { return BinaryenConst(module, BinaryenLiteralFloat32(x)); }
BinaryenExpressionRef BinaryenConstFloat64(BinaryenModuleRef module, double x) { return BinaryenConst(module, BinaryenLiteralFloat64(x)); }
BinaryenExpressionRef BinaryenConstVec128(BinaryenModuleRef module, const uint8_t x[16]) { return BinaryenConst(module, BinaryenLiteralVec128(x)); }
BinaryenExpressionRef BinaryenConstFloat32Bits(BinaryenModuleRef module, int32_t x) { return BinaryenConst(module, BinaryenLiteralFloat32Bits(x)); }
BinaryenExpressionRef BinaryenConstFloat64Bits(BinaryenModuleRef module, int64_t x) { return BinaryenConst(module, BinaryenLiteralFloat64Bits(x)); }

void BinaryenModuleAllocateAndWriteMut(BinaryenModuleRef module, const char* sourceMapUrl, void** binary, size_t* binaryBytes, char** sourceMap) {
	BinaryenModuleAllocateAndWriteResult r = BinaryenModuleAllocateAndWrite(module, sourceMapUrl);
	*binary = r.binary;
	*binaryBytes = r.binaryBytes;
	*sourceMap = r.sourceMap;
}
