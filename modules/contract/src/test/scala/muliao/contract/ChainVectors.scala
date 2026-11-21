package muliao.contract

/** 对照向量（#1092）：ethers 6、@solana/web3.js 1、tweetnacl 生成（生成脚本见 issue #1092 的评论）。 */
object ChainVectors:
  val json: String = """{
 "ethAddress": "0x2c7536E3605D9C16a7a3D7b1898e529396a65c23",
 "eip1559": {
  "unsignedHash": "0xb0f7fd996f8d04a3aab8ffddd315e9982cc3897f06be4b1e708d4fc15525fa4b",
  "raw": "0x02f87301078459682f008506fc23ac00825208943535353535353535353535353535353535353535880de0b6b3a764000080c001a02b85fc755ea20c385d0686fa3c943ce504bef9794a4106545f15f45728186152a06e04219535948f1208aac36405a6dadc608749dbc831949c815e594b6f4c5f05",
  "hash": "0xbdde58b328b81ff88b3235632514c1b6fc3c4c7ad55e374c98b937a997816589"
 },
 "eip1559b": {
  "unsignedHash": "0x0293f8123d91750955bcf693b9df240cab7ca89298ddc3da3af86f5814f7973c",
  "raw": "0x02f8e68221058001831e8480830186a094a0b86991c6218b36c1d19d4a2e9eb0ce3606eb4880b844a9059cbb0000000000000000000000003535353535353535353535353535353535353535000000000000000000000000000000000000000000000000000000000000000af838f794a0b86991c6218b36c1d19d4a2e9eb0ce3606eb48e1a0000000000000000000000000000000000000000000000000000000000000000180a0a2b4e6cfb7ba9063cbf60dd1d0091d31876921f3be3534fee71e21e93ea76b88a0263cec4b35ee8078b5e88898db4c7d4e53780c7e8207589226b09f86ef4d7ea1",
  "hash": "0xc064531b406676589377da6492f5a21b3ab7f584c9aecb947c6a172583bb3877"
 },
 "eip1559c": {
  "unsignedHash": "0x1e82d98ed5490a345a54cd777e1db4d19cd3d64502d74f814c950c8ae3a09273",
  "raw": "0x02f85583aa36a782012c800182cf088080826000c001a0737797779c00c5af55a64251f4c5a47ec52237986641026ed5a0bf297ba6e37ca0244ec5a8930444958964816d7c0d45aa495c50768e3178ce53c2335b3918c8bc",
  "hash": "0xa7aac9341c4dc1fbf8dd904fe6d66eb0bcea81540e3dc0d6b00167e89da7f8de"
 },
 "legacy": {
  "unsignedHash": "0xdaf5a779ae972f972197303d7b574746c7ef83eadac0f2791ad23db92e4c8e53",
  "raw": "0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a0499aa1110848b179aa0f228e20faa3ba68b350e1feeab49638c6b8ce40ea56aea0053ec9b43dcea26f8d10b43a44bdfafae5b1b26462367921079005d4d274e06d",
  "hash": "0xe4e0d6b0c5b43efcf6651888cf149d88384f0762ff684a58a06f8997b6e1f979"
 },
 "personalText": {
  "message": "hello muliao 你好",
  "hash": "0x7348f9c964e44886cde746a8145bbcc5886f038c17082413eaf350c405775d2c",
  "sig": "0x9a3e76ec10500fd635387504899e5b8334e8c349363031bb7862956b548d44601b068efea134c01c8e445c26827e29d56c99fa86e18b5e5b9ef832c2bd7149041c"
 },
 "personalHex": {
  "message": "0xdeadbeef00ff",
  "hash": "0x904a96ff822fef5be0815b58dfd6dc2d87ba58d99d04c400d77a062e174616cd",
  "sig": "0xe8c7197af8695b38996235f09ee961c3ad48125363120a658f0a8513f987ca5f21498a06d42005fba965e5a6df08846b37f06ca800671a617a6336e1d89b09f11b"
 },
 "mail": {
  "typedData": "{\"types\":{\"EIP712Domain\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"version\",\"type\":\"string\"},{\"name\":\"chainId\",\"type\":\"uint256\"},{\"name\":\"verifyingContract\",\"type\":\"address\"}],\"Person\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"wallet\",\"type\":\"address\"}],\"Mail\":[{\"name\":\"from\",\"type\":\"Person\"},{\"name\":\"to\",\"type\":\"Person\"},{\"name\":\"contents\",\"type\":\"string\"}]},\"primaryType\":\"Mail\",\"domain\":{\"name\":\"Ether Mail\",\"version\":\"1\",\"chainId\":1,\"verifyingContract\":\"0xCcCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC\"},\"message\":{\"from\":{\"name\":\"Cow\",\"wallet\":\"0xCD2a3d9F938E13CD947Ec05AbC7FE734Df8DD826\"},\"to\":{\"name\":\"Bob\",\"wallet\":\"0xbBbBBBBbbBBBbbbBbbBbbbbBBbBbbbbBbBbbBBbB\"},\"contents\":\"Hello, Bob!\"}}",
  "digest": "0xbe609aee343fb3c4b28e1df9e632fca64fcfaede20f02e86244efddf30957bd2",
  "sig": "0x19e4c5dcf77c10f4b30df3d4cb5822a99a69ec18e6f5d7b9a889a5a4815740732656ebdcc5a9c8eee63e684a5ed282d6b59ffe2914076b025a77df76559403111c"
 },
 "safe": {
  "typedData": "{\"types\":{\"EIP712Domain\":[{\"name\":\"chainId\",\"type\":\"uint256\"},{\"name\":\"verifyingContract\",\"type\":\"address\"}],\"SafeTx\":[{\"name\":\"to\",\"type\":\"address\"},{\"name\":\"value\",\"type\":\"uint256\"},{\"name\":\"data\",\"type\":\"bytes\"},{\"name\":\"operation\",\"type\":\"uint8\"},{\"name\":\"safeTxGas\",\"type\":\"uint256\"},{\"name\":\"baseGas\",\"type\":\"uint256\"},{\"name\":\"gasPrice\",\"type\":\"uint256\"},{\"name\":\"gasToken\",\"type\":\"address\"},{\"name\":\"refundReceiver\",\"type\":\"address\"},{\"name\":\"nonce\",\"type\":\"uint256\"}]},\"primaryType\":\"SafeTx\",\"domain\":{\"chainId\":\"0x1\",\"verifyingContract\":\"0x1111111111111111111111111111111111111111\"},\"message\":{\"to\":\"0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48\",\"value\":\"0\",\"data\":\"0xa9059cbb0000000000000000000000003535353535353535353535353535353535353535000000000000000000000000000000000000000000000000000000000000000a\",\"operation\":0,\"safeTxGas\":\"0\",\"baseGas\":0,\"gasPrice\":\"0\",\"gasToken\":\"0x0000000000000000000000000000000000000000\",\"refundReceiver\":\"0x0000000000000000000000000000000000000000\",\"nonce\":42}}",
  "digest": "0x264fcc5d1494e21ec555072d036b07ef03badc3e734465f64b62e4bac5571103",
  "sig": "0x6dbf9b0611bb02b8081b6f181b2f50d34dc1d9ab7fadbea4ee6f613e0f073620493c5cef38da78ba79d88099dc7c085f70521c8620204bdee2c304f764d19fde1c"
 },
 "complex": {
  "typedData": "{\"types\":{\"EIP712Domain\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"chainId\",\"type\":\"uint256\"},{\"name\":\"salt\",\"type\":\"bytes32\"}],\"Item\":[{\"name\":\"id\",\"type\":\"uint64\"},{\"name\":\"tags\",\"type\":\"string[]\"}],\"Order\":[{\"name\":\"maker\",\"type\":\"address\"},{\"name\":\"items\",\"type\":\"Item[]\"},{\"name\":\"pair\",\"type\":\"uint16[2]\"},{\"name\":\"delta\",\"type\":\"int256\"},{\"name\":\"small\",\"type\":\"int8\"},{\"name\":\"flag\",\"type\":\"bool\"},{\"name\":\"hash\",\"type\":\"bytes32\"},{\"name\":\"blob\",\"type\":\"bytes\"},{\"name\":\"short\",\"type\":\"bytes4\"},{\"name\":\"matrix\",\"type\":\"uint8[][]\"},{\"name\":\"zebra\",\"type\":\"Zebra\"}],\"Zebra\":[{\"name\":\"a\",\"type\":\"Item\"}]},\"primaryType\":\"Order\",\"domain\":{\"name\":\"Complex\",\"chainId\":137,\"salt\":\"0xabababababababababababababababababababababababababababababababab\"},\"message\":{\"maker\":\"0x3535353535353535353535353535353535353535\",\"items\":[{\"id\":\"18446744073709551615\",\"tags\":[\"a\",\"\",\"中文\"]},{\"id\":1,\"tags\":[]}],\"pair\":[1,65535],\"delta\":\"-123456789012345678901234567890\",\"small\":-128,\"flag\":true,\"hash\":\"0xcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd\",\"blob\":\"0x\",\"short\":\"0x01020304\",\"matrix\":[[1,2],[],[3]],\"zebra\":{\"a\":{\"id\":5,\"tags\":[\"z\"]}}}}",
  "digest": "0xdec1baf67a0795a3634eed2a010c9acc3b035b1b811800b6aa5a2a9d66666700",
  "sig": "0x025a00a08980b83cd5a0bd54caf0fec73e66a960089cf88e039d0c60812c8c3a24421af080e7d9c930fbe67aa6192b0d105b77cecfd4e3a7c1014c8780f109191c"
 },
 "solana": {
  "secretKeyJson": "[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,121,181,86,46,143,230,84,249,64,120,177,18,232,169,139,167,144,31,133,58,230,149,190,215,224,227,145,11,173,4,150,100]",
  "secretKeyB58": "2Ana1pUpv2ZbMVkwF5FXapYeBEjdxDatLn7nvJkhgTSdZd8hbDHTd21as7EAsg7ypityqfsw2pMQKJcVDVcAEsd",
  "publicKey": "9C6hybhQ6Aycep9jaUnP6uL9ZYvDjUp1aSkFWPUFJtpj"
 },
 "solanaLegacy": {
  "unsigned": "AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAAIEebVWLo/mVPlAeLES6KmLp5AfhTrmlb7X4OORC60ElmQLvDRqV2Z8OAEgvZx/1+UdLF/f6jfNL1v0BbLGv28teAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwZGb+UhFzL/7K26csOb57yM5bvF9xJrLEObOkAAAADIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5wIDAAkD6AMAAAAAAAACAgABDAIAAADSApZJAAAAAA==",
  "signed": "AUOJuiF14BkBAR9NpOm3JsZtRA0XJqo3dJ8CWiut/kUdefTpojqzumRhuvzYDsRlqtoAd5OFY8TOUl+0FLqd2A0BAAIEebVWLo/mVPlAeLES6KmLp5AfhTrmlb7X4OORC60ElmQLvDRqV2Z8OAEgvZx/1+UdLF/f6jfNL1v0BbLGv28teAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwZGb+UhFzL/7K26csOb57yM5bvF9xJrLEObOkAAAADIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5wIDAAkD6AMAAAAAAAACAgABDAIAAADSApZJAAAAAA==",
  "signature": "2MKQuPAVd6Uo7m7yu2DaFFEDCPUwBZwHtkdsEKYiABhYMSEhd3rWsVP6oXyAVUC6Xtrz36XCQhEUskLcBzXb6G5a"
 },
 "solanaV0": {
  "unsigned": "AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAQABAnm1Vi6P5lT5QHixEuipi6eQH4U65pW+1+DjkQutBJZkAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5wEBAgACDAIAAAAFAAAAAAAAAAHqSmxj4pxSCr71UHsTLsX5lUd2rr6+e5JCHuppFEbSLAEAAA==",
  "signed": "AVwsjD4WEFkhWOcMK+ByQlz1T6JuiDYTlP3Du2LOT1FUpPABMQJeTyFThRxaJpo21fqylS6KaptRX3pMYrJH9waAAQABAnm1Vi6P5lT5QHixEuipi6eQH4U65pW+1+DjkQutBJZkAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5wEBAgACDAIAAAAFAAAAAAAAAAHqSmxj4pxSCr71UHsTLsX5lUd2rr6+e5JCHuppFEbSLAEAAA==",
  "signature": "2qtMtX8McFf6C5D21a4291Jt2EkzJHQrCpMohveQ1G5Lq6eWdHNTQJnpnsPoSKXeKSDr7PzY9GXFfJRj6UHWMgs7"
 },
 "solanaMulti": {
  "unsigned": "Aq6FZTFd4ZocxehIPRpJxw7E4gf6zflxjc2mPZPgSaoK/tC3EOlT0BieDpSkhYw3JIr31NmRglkMR0oaRpsDJw0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgABBO1JKMYo0cLG6ukDOJBZlWEpWSc6XGP5NjbBRhSshzfRebVWLo/mVPlAeLES6KmLp5AfhTrmlb7X4OORC60ElmQLvDRqV2Z8OAEgvZx/1+UdLF/f6jfNL1v0BbLGv28teAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAyMnKy8zNzs/Q0dLT1NXW19jZ2tvc3d7f4OHi4+Tl5ucBAwIBAgwCAAAAAQAAAAAAAAA=",
  "signed": "Aq6FZTFd4ZocxehIPRpJxw7E4gf6zflxjc2mPZPgSaoK/tC3EOlT0BieDpSkhYw3JIr31NmRglkMR0oaRpsDJw2TLvJTrsLNvCPuMZGMkjeJzFZQ5XfrPlL+fZR7aMipTU6ntLTZAnRmRnnifKDTXEmJRJsnRnY9Ry4TrtBouOUOAgABBO1JKMYo0cLG6ukDOJBZlWEpWSc6XGP5NjbBRhSshzfRebVWLo/mVPlAeLES6KmLp5AfhTrmlb7X4OORC60ElmQLvDRqV2Z8OAEgvZx/1+UdLF/f6jfNL1v0BbLGv28teAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAyMnKy8zNzs/Q0dLT1NXW19jZ2tvc3d7f4OHi4+Tl5ucBAwIBAgwCAAAAAQAAAAAAAAA=",
  "signature": "3wg8hd7y4MokF4QThxMzYRaLoopkseFdn9ChkM7ourFMrHs1RQjBSNiFT4NLia4shznnQZ1YosoiAqg5pDyh314H"
 },
 "solanaMessage": {
  "message": "Sign in to example.com\nnonce: 42",
  "signature": "5KBB5jCxhounni1YZY3UWNGz26DvNazgjuh6txTvsQRwPNVmkMQLV3kHvtrHsQjp2jxfdkqeowjLPSKLd3cdNxQf"
 },
 "keccak": {
  "0": "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470",
  "3": "0xb9a5dc0048db9a7d13548781df3cd4b2334606391f75f40c14225a92f4cb3537",
  "135": "0x34367dc248bbd832f4e3e69dfaac2f92638bd0bbd18f2912ba4ef454919cf446",
  "136": "0xa6c4d403279fe3e0af03729caada8374b5ca54d8065329a3ebcaeb4b60aa386e",
  "137": "0xd869f639c7046b4929fc92a4d988a8b22c55fbadb802c0c66ebcd484f1915f39",
  "300": "0x5b7e0e47a96f32a88b4f14ca177982790807c40e1a105742ba0fc1babe1ef826"
 },
 "sigs": {
  "eip1559": {
   "r": "0x2b85fc755ea20c385d0686fa3c943ce504bef9794a4106545f15f45728186152",
   "s": "0x6e04219535948f1208aac36405a6dadc608749dbc831949c815e594b6f4c5f05",
   "yParity": 1
  },
  "eip1559b": {
   "r": "0xa2b4e6cfb7ba9063cbf60dd1d0091d31876921f3be3534fee71e21e93ea76b88",
   "s": "0x263cec4b35ee8078b5e88898db4c7d4e53780c7e8207589226b09f86ef4d7ea1",
   "yParity": 0
  },
  "eip1559c": {
   "r": "0x737797779c00c5af55a64251f4c5a47ec52237986641026ed5a0bf297ba6e37c",
   "s": "0x244ec5a8930444958964816d7c0d45aa495c50768e3178ce53c2335b3918c8bc",
   "yParity": 1
  },
  "legacy": {
   "r": "0x499aa1110848b179aa0f228e20faa3ba68b350e1feeab49638c6b8ce40ea56ae",
   "s": "0x053ec9b43dcea26f8d10b43a44bdfafae5b1b26462367921079005d4d274e06d",
   "yParity": 0
  }
 },
 "checksum": "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359",
 "ethKey": "0x4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318"
}"""
