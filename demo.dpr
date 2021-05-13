Program demo;

uses windows, sysutils;


////////////////////////////////////////////////////////////////////////////////
Type
  TAPAnsichar = array[0..0] of PAnsichar;
  PAPAnsichar = ^TAPAnsichar;

  PConstNum = ^TConstNum;
  TConstNum = Object
  Private
    FCount:LongWord;
    FItems:PPAnsichar;
  Protected
    Function GetItems(index:LongWord):PPAnsichar;
  Public
    Constructor Create(ACount:LongWord);
    property Count:LongWord read FCount;
    property Items[index:LongWord]:PPAnsichar read GetItems;// default;
  End;
  Constructor TConstNum.Create(ACount:LongWord);
  Begin
    FCount := ACount;
    GetMem(FItems, FCount*SizeOf(FItems^));
    FillChar(FItems^, FCount*SizeOf(FItems^),0);
  End;
  Function TConstNum.GetItems(index:LongWord):PPAnsichar;
  Begin
    if index>=Count then
    raise Exception.Create('error TConstNum.GetItems');
    Result := @PAPAnsichar(FItems)^[index];
  End;
////////////////////////////////////////////////////////////////////////////////
Function ReadConstNum(Const ConstNum:PConstNum; Const BufData:Pointer;Const BufSize:LongWord):LongWord;
Var
  N, I : LongWord;
  iData:array[0..1] of LongWord;
  dData:PDouble;
  tmpStr:AnsiString;

  fBuffData : PAnsiChar;
  fBuffSize : LongWord;
  AReturn   : LongWord absolute Result;
  Function Next(value:LongWord=1):Byte;
  Begin
    if value>fBuffSize then
    raise Exception.Create('ConstNum.Next = error size');
    Inc(AReturn,value);
    Result := Byte(fBuffData^);
    Dec(fBuffSize,value);
    Inc(fBuffData,value);
  End;
  Function next_uleb128:LongWord;
  Var
    sh:Integer;
  Begin
    sh := 0;
    Result := 0;
    while Byte(fBuffData^) >= $80 do
    Begin
      Result := Result or ((next and $7f) shl sh);
      sh := sh + 7;
    End;
    Result := Result or ((next and $7f) shl sh);
  End;
  Function next_uleb128_33:LongWord;
  Var
    sh:Integer;
  Begin
    if Byte(fBuffData^) >= $80 then
    Begin
      sh := 6;
      Result := (next shr 1) and $3f;
      while Byte(fBuffData^) >= $80 do
      Begin
        Result := Result or ((next and $7f) shl sh);
        sh := sh + 7;
      End;
      Result := Result or ((next and $7f) shl sh);
    End else
    Result := (next shr 1) and $3f;
  End;
Begin
  AReturn   := 0;
  fBuffSize := BufSize;
  fBuffData := BufData;
  dData := @iData;
  if (ConstNum=nil)or(BufData=nil) then
  raise Exception.Create('error ReadConstNum');

  for I := 1 to ConstNum^.FCount do
  Begin
    iData[1]:=0;
    if (Byte(fBuffData^)and 1)>0 Then
    Begin
      iData[0]:=next_uleb128_33;
      iData[1]:=next_uleb128;
      tmpStr := Format('%.f',[dData^]);
    End else
    //  dData^:=next_uleb128_33;
    //ConstNum^.Items[I-1]^:=dData^;
      tmpStr := Format('%.d',[next_uleb128_33]);
    N := Length(tmpStr);
    GetMem(ConstNum^.Items[I-1]^,N+1);
    Move(PAnsiChar(tmpStr)^,ConstNum^.Items[I-1]^^,N);
    ConstNum^.Items[I-1]^[N]:=#0;
  End;
End;
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
Type
  TAWord = array[0..0] of Word;
  PAWord = ^TAWord;

  PConstUV = ^TConstUV;
  TConstUV = Object
  Private
    FCount:LongWord;
    FItems:PWord;
  Protected
    Function GetItems(index:LongWord):PWord;
  Public
    Constructor Create(ACount:LongWord);
    property Count:LongWord read FCount;
    property Items[index:LongWord]:PWord read GetItems;
  End;
  Constructor TConstUV.Create(ACount:LongWord);
  Begin
    FCount := ACount;
    GetMem(FItems, FCount*SizeOf(FItems^));
    FillChar(FItems^, FCount*SizeOf(FItems^),0);
  End;
  Function TConstUV.GetItems(index:LongWord):PWord;
  Begin
    if index>=Count then
    raise Exception.Create('error TConstUV.GetItems');
    Result := @PAWord(FItems)^[index];
  End;
////////////////////////////////////////////////////////////////////////////////
Function ReadConstUV(Const ConstUV:PConstUV; Const BufData:Pointer;Const BufSize:LongWord):LongWord;
Var
  i:Integer;
  AReturn   : LongWord absolute Result;
Begin
  AReturn   := 0;
  if (ConstUV=nil)or(BufData=nil) then
  raise Exception.Create('error ReadConstUV');

  AReturn := SizeOf(ConstUV^.FItems^)*ConstUV^.FCount;
  if AReturn>BufSize then
  raise Exception.Create('error ReadConstUV');

  Move(BufData^, ConstUV^.FItems^,AReturn);

  for I := 1 to ConstUV^.FCount do
  Begin
    ConstUV^.Items[i-1]^ := word((ConstUV^.Items[i-1]^ shr 8)or(ConstUV^.Items[i-1]^ shl 8));
  End;
End;
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Bytecode instruction format, 32 bit wide, fields of 8 or 16 bit:
//
// +----+----+----+----+
// | B  | C  | A  | OP | Format ABC
// +----+----+----+----+
// |    D    | A  | OP | Format AD
// +--------------------
// MSB               LSB
//
// In-memory instructions are always stored in host byte order.
////////////////////////////////////////////////////////////////////////////////
Const
  // Operand ranges and related constants.
  BCMAX_A  = $ff;
  BCMAX_B  = $ff;
  BCMAX_C  = $ff;
  BCMAX_D  = $ffff;
  BCBIAS_J = $8000;
  NO_REG   = BCMAX_A;
  //NO_JMP		= (~(BCPos)0);
Type
  // Comparison ops. ORDER OPR.
  T_BCDEF = (
    BCDEF_ISLT  ,  BCDEF_ISGE  ,  BCDEF_ISLE  ,  BCDEF_ISGT  ,
    BCDEF_ISEQV ,  BCDEF_ISNEV ,  BCDEF_ISEQS ,  BCDEF_ISNES ,
    BCDEF_ISEQN ,  BCDEF_ISNEN ,  BCDEF_ISEQP ,  BCDEF_ISNEP ,
    BCDEF_ISTC  ,  BCDEF_ISFC  ,  BCDEF_IST   ,  BCDEF_ISF   ,

    BCDEF_MOV   ,  BCDEF_NOT   ,  BCDEF_UNM   ,  BCDEF_LEN   ,

    BCDEF_ADDVN ,  BCDEF_SUBVN ,  BCDEF_MULVN ,  BCDEF_DIVVN ,  BCDEF_MODVN ,
    BCDEF_ADDNV ,  BCDEF_SUBNV ,  BCDEF_MULNV ,  BCDEF_DIVNV ,  BCDEF_MODNV ,
    BCDEF_ADDVV ,  BCDEF_SUBVV ,  BCDEF_MULVV ,  BCDEF_DIVVV ,  BCDEF_MODVV ,

    BCDEF_POW   ,  BCDEF_CAT   ,

    BCDEF_KSTR  ,  BCDEF_KCDATA,  BCDEF_KSHORT,  BCDEF_KNUM  ,  BCDEF_KPRI  ,
    BCDEF_KNIL  ,

    BCDEF_UGET  ,  BCDEF_USETV ,  BCDEF_USETS ,  BCDEF_USETN ,  BCDEF_USETP ,
    BCDEF_UCLO  ,

    BCDEF_FNEW  ,  BCDEF_TNEW  ,  BCDEF_TDUP  ,

    BCDEF_GGET  ,  BCDEF_GSET  ,
    BCDEF_TGETV ,  BCDEF_TGETS ,  BCDEF_TGETB ,
    BCDEF_TSETV ,  BCDEF_TSETS ,  BCDEF_TSETB ,  BCDEF_TSETM ,

    BCDEF_CALLM ,  BCDEF_CALL  ,  BCDEF_CALLMT,  BCDEF_CALLT ,

    BCDEF_ITERC ,  BCDEF_ITERN ,  BCDEF_VARG  ,  BCDEF_ISNEXT,

    BCDEF_RETM  ,  BCDEF_RET   ,  BCDEF_RET0  ,  BCDEF_RET1  ,

    BCDEF_FORI  ,  BCDEF_JFORI ,  BCDEF_FORL  ,  BCDEF_IFORL ,  BCDEF_JFORL ,
    BCDEF_ITERL ,  BCDEF_IITERL,  BCDEF_JITERL,
    BCDEF_LOOP  ,  BCDEF_ILOOP ,  BCDEF_JLOOP ,
    BCDEF_JMP   ,

    BCDEF_FUNCF ,  BCDEF_IFUNCF,  BCDEF_JFUNCF,
    BCDEF_FUNCV ,  BCDEF_IFUNCV,  BCDEF_JFUNCV,  BCDEF_FUNCC ,
    BCDEF_FUNCCW
  );

  T_BCDEF_ARG = ( BCDEF_ARG_NONE, BCDEF_ARG_DST
                , BCDEF_ARG_PRI, BCDEF_ARG_VAR, BCDEF_ARG_NUM, BCDEF_ARG_STR, BCDEF_ARG_TAB
                , BCDEF_ARG_BASE, BCDEF_ARG_RBASE, BCDEF_ARG_CDATA, BCDEF_ARG_UV
                , BCDEF_ARG_LIT, BCDEF_ARG_LITS, BCDEF_ARG_JUMP, BCDEF_ARG_FUNC
                );
  T_BCDEF_ARG_SET = set of T_BCDEF_ARG;

  T_BCDEF_ITEM = Record
    OP:AnsiString;
    A:T_BCDEF_ARG;
    B:T_BCDEF_ARG;
    C:T_BCDEF_ARG;
    info:AnsiString;
  End;

Const
  T_BCDEF_INFO : array[T_BCDEF] of T_BCDEF_ITEM =(
  // Comparison ops. ORDER OPR.
  (OP:'ISLT'  ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'lt'),
  (OP:'ISGE'  ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'lt'),
  (OP:'ISLE'  ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'le'),
  (OP:'ISGT'  ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'le'),

  (OP:'ISEQV' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'eq'),
  (OP:'ISNEV' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'eq'),
  (OP:'ISEQS' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_STR;   info:'eq'),
  (OP:'ISNES' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_STR;   info:'eq'),
  (OP:'ISEQN' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NUM;   info:'eq'),
  (OP:'ISNEN' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NUM;   info:'eq'),
  (OP:'ISEQP' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_PRI;   info:'eq'),
  (OP:'ISNEP' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_PRI;   info:'eq'),

  // Unary test and copy ops.
  (OP:'ISTC'  ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'___'),
  (OP:'ISFC'  ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'___'),
  (OP:'IST'   ;  A:BCDEF_ARG_NONE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'___'),
  (OP:'ISF'   ;  A:BCDEF_ARG_NONE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'___'),

  // Unary ops.
  (OP:'MOV'   ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'___'),
  (OP:'NOT'   ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'___'),
  (OP:'UNM'   ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'unm'),
  (OP:'LEN'   ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'len'),

  // Binary ops. ORDER OPR. VV last; POW must be next.
  (OP:'ADDVN' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_NUM;   info:'add'),
  (OP:'SUBVN' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_NUM;   info:'sub'),
  (OP:'MULVN' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_NUM;   info:'mul'),
  (OP:'DIVVN' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_NUM;   info:'div'),
  (OP:'MODVN' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_NUM;   info:'mod'),

  (OP:'ADDNV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_NUM;   info:'add'),
  (OP:'SUBNV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_NUM;   info:'sub'),
  (OP:'MULNV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_NUM;   info:'mul'),
  (OP:'DIVNV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_NUM;   info:'div'),
  (OP:'MODNV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_NUM;   info:'mod'),

  (OP:'ADDVV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_VAR;   info:'add'),
  (OP:'SUBVV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_VAR;   info:'sub'),
  (OP:'MULVV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_VAR;   info:'mul'),
  (OP:'DIVVV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_VAR;   info:'div'),
  (OP:'MODVV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_VAR;   info:'mod'),

  (OP:'POW'   ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_VAR;   info:'pow'),
  (OP:'CAT'   ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_RBASE; C:BCDEF_ARG_RBASE; info:'concat'),

  // Constant ops.
  (OP:'KSTR'  ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_STR ;  info:'___'),
  (OP:'KCDATA';  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_CDATA; info:'___'),
  (OP:'KSHORT';  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LITS;  info:'___'),
  (OP:'KNUM'  ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NUM;   info:'___'),
  (OP:'KPRI'  ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_PRI;   info:'___'),
  (OP:'KNIL'  ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_BASE;  info:'___'),

  // Upvalue and function ops.
  (OP:'UGET'  ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_UV;    info:'___'),
  (OP:'USETV' ;  A:BCDEF_ARG_UV;    B:BCDEF_ARG_NONE;  C:BCDEF_ARG_VAR;   info:'___'),
  (OP:'USETS' ;  A:BCDEF_ARG_UV;    B:BCDEF_ARG_NONE;  C:BCDEF_ARG_STR;   info:'___'),
  (OP:'USETN' ;  A:BCDEF_ARG_UV;    B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NUM;   info:'___'),
  (OP:'USETP' ;  A:BCDEF_ARG_UV;    B:BCDEF_ARG_NONE;  C:BCDEF_ARG_PRI;   info:'___'),
  (OP:'UCLO'  ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),
  (OP:'FNEW'  ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_FUNC;  info:'gc'),

  // Table ops.
  (OP:'TNEW'  ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'gc'),
  (OP:'TDUP'  ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_TAB;   info:'gc'),
  (OP:'GGET'  ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_STR;   info:'index'),
  (OP:'GSET'  ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_NONE;  C:BCDEF_ARG_STR;   info:'newindex'),
  (OP:'TGETV' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_VAR;   info:'index'),
  (OP:'TGETS' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_STR;   info:'index'),
  (OP:'TGETB' ;  A:BCDEF_ARG_DST;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_LIT;   info:'index'),
  (OP:'TSETV' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_VAR;   info:'newindex'),
  (OP:'TSETS' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_STR;   info:'newindex'),
  (OP:'TSETB' ;  A:BCDEF_ARG_VAR;   B:BCDEF_ARG_VAR;   C:BCDEF_ARG_LIT;   info:'newindex'),
  (OP:'TSETM' ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NUM;   info:'newindex'),

  // Calls and vararg handling. T = tail call.
  (OP:'CALLM' ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_LIT;   C:BCDEF_ARG_LIT;   info:'call'),
  (OP:'CALL'  ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_LIT;   C:BCDEF_ARG_LIT;   info:'call'),
  (OP:'CALLMT';  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'call'),
  (OP:'CALLT' ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'call'),
  (OP:'ITERC' ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_LIT;   C:BCDEF_ARG_LIT;   info:'call'),
  (OP:'ITERN' ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_LIT;   C:BCDEF_ARG_LIT;   info:'call'),
  (OP:'VARG'  ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_LIT;   C:BCDEF_ARG_LIT;   info:'___'),
  (OP:'ISNEXT';  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),

  // Returns.
  (OP:'RETM'  ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'___'),
  (OP:'RET'   ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'___'),
  (OP:'RET0'  ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'___'),
  (OP:'RET1'  ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'___'),

  // Loops and branches. I/J = interp/JIT; I/C/L = init/call/loop.
  (OP:'FORI'  ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),
  (OP:'JFORI' ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),

  (OP:'FORL'  ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),
  (OP:'IFORL' ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),
  (OP:'JFORL' ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'___'),

  (OP:'ITERL' ;  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),
  (OP:'IITERL';  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),
  (OP:'JITERL';  A:BCDEF_ARG_BASE;  B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'___'),

  (OP:'LOOP'  ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),
  (OP:'ILOOP' ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),
  (OP:'JLOOP' ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'___'),

  (OP:'JMP'   ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_JUMP;  info:'___'),

  // Function headers. I/J = interp/JIT; F/V/C = fixarg/vararg/C func.
  (OP:'FUNCF' ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NONE;  info:'___'),
  (OP:'IFUNCF';  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NONE;  info:'___'),
  (OP:'JFUNCF';  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'___'),
  (OP:'FUNCV' ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NONE;  info:'___'),
  (OP:'IFUNCV';  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NONE;  info:'___'),
  (OP:'JFUNCV';  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_LIT;   info:'___'),
  (OP:'FUNCC' ;  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NONE;  info:'___'),
  (OP:'FUNCCW';  A:BCDEF_ARG_RBASE; B:BCDEF_ARG_NONE;  C:BCDEF_ARG_NONE;  info:'___')
  );

Type

  PBlockData = ^TBlockData;
  TBlockData = Object
    OpCode:T_BCDEF;
    FunRun:Byte;
    ValuHi:Byte;
    ValuLo:Byte;
  End;
  TABlockData = array[0..0] of TBlockData;
  PABlockData = ^TABlockData;

  PCodeData = ^TCodeData;
  TCodeData = Object
  Private
    FCount:LongWord;
    FItems:PBlockData;
  Protected
    Function GetItems(index:LongWord):PBlockData;
  Public
    Constructor Create(ACount:LongWord);
    property Count:LongWord read FCount;
    property Items[index:LongWord]:PBlockData read GetItems;// default;
  End;
  Constructor TCodeData.Create(ACount:LongWord);
  Begin
    FCount := ACount;
    GetMem(FItems, FCount*SizeOf(FItems^));
    FillChar(FItems^, FCount*SizeOf(FItems^),0);
  End;
  Function TCodeData.GetItems(index:LongWord):PBlockData;
  Begin
    if index>=Count then
    raise Exception.Create('error TCodeData.GetItems');
    Result := @PABlockData(FItems)^[index];
  End;
////////////////////////////////////////////////////////////////////////////////
Function ReadCodeData(Const CodeData:PCodeData; Const BufData:Pointer;Const BufSize:LongWord):LongWord;
Begin
  if (CodeData=nil)or(BufData=nil) then
  raise Exception.Create('error ReadResource');

  Result := CodeData^.FCount*SizeOf(CodeData^.FItems^);

  if Result>BufSize then
  raise Exception.Create('error ReadResource');

  Move(BufData^,CodeData^.FItems^, Result);
End;
////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////
Const
  C_BCDUMP_KTAB_NIL    = 0;
  C_BCDUMP_KTAB_FALSE  = 1;
  C_BCDUMP_KTAB_TRUE   = 2;
  C_BCDUMP_KTAB_INT    = 3;
  C_BCDUMP_KTAB_NUM    = 4;
  C_BCDUMP_KTAB_STR    = 5;

  C_BCDUMP_KGC_CHILD   = 0;
  C_BCDUMP_KGC_TAB     = 1;
  C_BCDUMP_KGC_I64     = 2;
  C_BCDUMP_KGC_U64     = 3;
  C_BCDUMP_KGC_COMPLEX = 4;
  C_BCDUMP_KGC_STR     = 5;
Type
  // Type codes for the keys/values of a constant table.
  T_BCDUMP_KTAB = (BCDUMP_KTAB_NIL, BCDUMP_KTAB_FALSE, BCDUMP_KTAB_TRUE,
                   BCDUMP_KTAB_INT, BCDUMP_KTAB_NUM  , BCDUMP_KTAB_STR);
  //Type codes for the GC constants of a prototype. Plus length for strings.
  T_BCDUMP_KGC  = (BCDUMP_KGC_CHILD, BCDUMP_KGC_TAB, BCDUMP_KGC_I64,
                   BCDUMP_KGC_U64, BCDUMP_KGC_COMPLEX, BCDUMP_KGC_STR);
  //////////////////////////////////////////////////////////////////////////////
  PResourceData = ^TResourceData;
  TResourceData = Object
  Private
    FTypes:T_BCDUMP_KGC;
    FCount:LongWord;
    FItems:PResourceData;
    FSizes:LongWord;
    FValue:PAnsiChar;
  Protected
    Function GetItems(index:Longword):PResourceData;
  Public
    Constructor CreateSTR(AData:PAnsiChar; ASize:LongWord);
    Constructor CreateTAB(ACount:LongWord);
    Constructor CreateCHILD(ACount:LongWord);

    property Types:T_BCDUMP_KGC read FTypes;

    property Count:LongWord read FCount;
    property Items[index:Longword]:PResourceData read GetItems;// default;

    property Sizes:LongWord read FSizes;
    property Value:PAnsiChar read FValue;
  End;
  TAResourceData = array[0..0]of TResourceData;
  PAResourceData = ^TAResourceData;

  Constructor TResourceData.CreateSTR(AData:PAnsiChar; ASize:LongWord);
  Begin
    FCount := 0;
    FItems := nil;
    FTypes := BCDUMP_KGC_STR;
    FSizes := ASize;
    GetMem(FValue,FSizes+1);FValue[FSizes]:=#0;
    Move(AData^,FValue^,FSizes);
  End;
  Constructor TResourceData.CreateTAB(ACount:LongWord);
  Begin
    FSizes := 0;
    FValue := nil;
    FTypes := BCDUMP_KGC_TAB;
    FCount := ACount;
    GetMem(FItems, FCount*SizeOf(FItems^));
    FillChar(FItems^, FCount*SizeOf(FItems^),0);
  End;
  Constructor TResourceData.CreateCHILD(ACount:LongWord);
  Begin
    FSizes := 0;
    FValue := nil;
    FTypes := BCDUMP_KGC_CHILD;
    FCount := ACount;
    GetMem(FItems, FCount*SizeOf(FItems^));
    FillChar(FItems^, FCount*SizeOf(FItems^),0);
  End;
  Function TResourceData.GetItems(index:Longword):PResourceData;
  Begin
    if (index>=Count)or((FTypes <> BCDUMP_KGC_TAB)and(FTypes <> BCDUMP_KGC_CHILD)) then
    raise Exception.Create('error TResourceData.GetItems');
    Result := @PAResourceData(FItems)^[index];
  End;
  //////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
Function ReadResource(Const Resource:PResourceData; Const BufData:Pointer;Const BufSize:LongWord; revert:boolean=false):LongWord;
Var
  I,J,N:Integer;
  nArray, nHash :LongWord;

  iData:array[0..1] of LongWord;
  fData:PDouble;
  tmp:AnsiString;
  ResultSize:LongWord absolute result;

  DataBuff:PansiChar;
  DataSize:LongWord;
  // Текущий символ из Буфера
  CharData:PAnsiChar;
  // Следующий символ из Буфера
  CharNext:AnsiChar;
  CharSize:LongWord;
  CharFlag:LongWord;

  DataChar:array[0..$ffff] of AnsiChar;
  Function Next(ACount:LongWord=1):Byte;
  Begin
    Inc(ResultSize,ACount);
    Result:=Byte(CharData^);
    Dec(CharSize,ACount);
    Inc(CharData,ACount);
    CharNext:=CharData[1];
  End;

  Procedure Clear;
  Begin
    DataSize:=0;
    FillChar(DataChar,SizeOf(DataChar),0);
  End;

  Procedure SetDataChar(value:AnsiChar);
  Begin
    DataChar[DataSize]:=value;
    Inc(DataSize);
  End;

  Procedure CopyDataChar(value:LongWord);
  var V:AnsiChar;
  Begin
    if value>CharSize then
    raise Exception.Create('error CopyDataChar');

    while value<>0 do
    Begin
      V:=AnsiChar(Next);
      case V of
        #10:
        Begin
          SetDataChar('\');
          SetDataChar('n');
        End;
        '"','\','''':
        Begin
          SetDataChar('\');
          SetDataChar(V);
        End;
      else
        SetDataChar(V);
      end;
      value := value -1;
    End;
  End;
  //Загружаем значение параметра текст "......"
  Procedure MoveDataText(Values:PAnsiChar);
  var fN:Integer;
  Begin
    fN:=0;
    while Values[fN]<>#0 do
    Begin
      SetDataChar(Values[fN]);
      fN := fN + 1;
    End;
  End;

  Function next_uleb128:LONGLONG;
  Var sh:Integer;
  Begin
    sh := 0;
    Result := 0;
    while Byte(CharData^) >= $80 do
    Begin
      Result := Result or ((next and $7f) shl sh);
      sh := sh + 7;
    End;
    Result := Result or ((next and $7f) shl sh);
  End;
  Procedure Read_kTab;
  Begin
      if Resource^.Types=BCDUMP_KGC_TAB then
      Begin
        CharFlag := next_uleb128;
        if CharFlag >= C_BCDUMP_KTAB_STR then
        Begin
          MoveDataText(' = "'#0);
          CopyDataChar(CharFlag - C_BCDUMP_KGC_STR);
          SetDataChar('"');
        End else
        if CharFlag = C_BCDUMP_KTAB_NIL then
        Begin
          MoveDataText(' = null'#0);
        End else
        if CharFlag = C_BCDUMP_KTAB_FALSE then
        Begin
          MoveDataText(' = false'#0);
        End else
        if CharFlag = C_BCDUMP_KTAB_TRUE then
        Begin
          MoveDataText(' = true'#0);
        End else
        if CharFlag = C_BCDUMP_KTAB_INT then
        Begin
          tmp := Format(' = %.d',[next_uleb128]);
          MoveDataText(PAnsiChar(tmp));
        End else
        if CharFlag = C_BCDUMP_KTAB_NUM then
        Begin
          iData[0]:=next_uleb128;
          iData[1]:=next_uleb128;
          tmp := Format(' = %f',[fData^]);
          tmp := StringReplace(tmp,',','.',[rfReplaceAll]);
          MoveDataText(PAnsiChar(tmp));
        End else
        raise Exception.Create('error BCDUMP_KTAB');
      End;
  End;
Begin
  Clear;
  DataBuff:=@DataChar;

  CharSize:=BufSize;
  CharData:=BufData;
  CharNext:=CharData[1];
  ResultSize := 0;
  fData := @iData;

  for I := 1 to Resource^.Count do
  Begin
    if not revert then J := I-1 else
    J := Resource^.Count-I;
    CharFlag := next_uleb128;

    // C_BCDUMP_KGC_TAB -----------------------------------------
    if CharFlag >= C_BCDUMP_KGC_STR then
    Begin
      CopyDataChar(CharFlag - C_BCDUMP_KGC_STR);
      Read_kTab;
      Resource^.Items[J]^.CreateSTR(DataBuff,DataSize);
      Clear;
    end else
    // C_BCDUMP_KGC_I64 -----------------------------------------
    if CharFlag = C_BCDUMP_KGC_I64 then
    Begin
      Clear;
      tmp := Format('%.d',[next_uleb128]);
      MoveDataText(PAnsiChar(tmp));
      Read_kTab;
      Resource^.Items[J]^.CreateSTR(DataBuff,DataSize);
      Clear;
    End else
    // C_BCDUMP_KGC_U64 -----------------------------------------
    if CharFlag = C_BCDUMP_KGC_U64 then
    Begin
      Clear;
      tmp := Format('%.d',[next_uleb128]);
      MoveDataText(PAnsiChar(tmp));
      Read_kTab;
      Resource^.Items[J]^.CreateSTR(DataBuff,DataSize);
      Clear;
    End else
    // C_BCDUMP_KGC_COMPLEX -------------------------------------
    if CharFlag = C_BCDUMP_KGC_COMPLEX then
    Begin
      Clear;
      iData[0]:=next_uleb128;
      iData[1]:=next_uleb128;
      tmp := Format('%f',[fData^]);
      tmp := StringReplace(tmp,',','.',[rfReplaceAll]);
      MoveDataText(PAnsiChar(tmp));
      Read_kTab;
      Resource^.Items[J]^.CreateSTR(DataBuff,DataSize);
      Clear;
    End else
    // C_BCDUMP_KGC_CHILD ---------------------------------------
    if CharFlag = C_BCDUMP_KGC_CHILD then
    Begin
      Resource^.Items[J]^.CreateCHILD(0);
      if Resource^.Types=BCDUMP_KGC_TAB then
      raise Exception.Create('error BCDUMP_KTAB');
    End else
    // C_BCDUMP_KGC_TAB -----------------------------------------
    if CharFlag = C_BCDUMP_KGC_TAB then
    Begin
      nArray := next_uleb128;
      nHash  := next_uleb128;
      if (nArray>0)and(nHash>0) then
      raise Exception.Create('error BCDUMP_KTAB');
      if nArray>0 then
         Resource^.Items[J]^.CreateCHILD(nArray);
      if nHash>0 then
         Resource^.Items[J]^.CreateTAB(nHash);
      N := ReadResource(Resource^.Items[J],CharData,CharSize);
      Next(N);
    End else
    //-----------------------------------------------------------
    raise Exception.Create('error BCDUMP_KTAB');
  End;
End;
////////////////////////////////////////////////////////////////////////////////
Function TextResource(Resource:PResourceData; TurnStep:LongWord=0; BaseList:boolean=false):AnsiString;
Var I:Integer;tmp:AnsiString;
Begin
  Result := '';
  if Resource=nil then exit;

  for I := 1 to Resource^.Count do
  Begin

    if Resource^.Types = BCDUMP_KGC_CHILD Then
    Begin
      if BaseList then
      Begin
        if Result<>'' then Result := Result + #13#10;
        Result := Result + Format('%-*s[%.2d] = ',[TurnStep,'',I - 1])
      End else
      Begin
        if Result<>'' then Result := Result + ', ';
      End;

    End Else
    if Resource^.Types = BCDUMP_KGC_TAB Then
    Begin
      if Result<>'' then Result := Result + ', ';
    End;

    if Resource^.Items[I - 1]^.Types = BCDUMP_KGC_STR Then
    Begin
       tmp := Resource^.Items[I - 1]^.Value;
    End else
    if Resource^.Items[I - 1]^.Types = BCDUMP_KGC_TAB Then
    Begin
      tmp := '{'+TextResource(Resource^.Items[I - 1], TurnStep+2)+'}';
    End else
    if Resource^.Items[I - 1]^.Types = BCDUMP_KGC_CHILD Then
    Begin
      //tmp := TextResource(Resource^.Items[I - 1], TurnStep+6);
      //if tmp<>'' then tmp := #13#10 + tmp;
      if Resource^.Items[I - 1]^.Count=0 then tmp := '0' else
      tmp := '{'+TextResource(Resource^.Items[I - 1], TurnStep+2)+'}';
    End else
    raise Exception.Create('error BCDUMP_KTAB');

    Result := Result + tmp;
  End;

End;
////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////
// Bytecode dump format
////////////////////////////////////////////////////////////////////////////////
//** dump   = header proto+ 0U
//** header = ESC 'L' 'J' versionB flagsU [namelenU nameB*]
//** proto  = lengthU pdata
//** pdata  = phead bcinsW* uvdataH* kgc* knum* [debugB*]
//** phead  = flagsB numparamsB framesizeB numuvB numkgcU numknU numbcU
//**          [debuglenU [firstlineU numlineU]]
//** kgc    = kgctypeU { ktab | (loU hiU) | (rloU rhiU iloU ihiU) | strB* }
//** knum   = intU0 | (loU1 hiU)
//** ktab   = narrayU nhashU karray* khash*
//** karray = ktabk
//** khash  = ktabk ktabk
//** ktabk  = ktabtypeU { intU | (loU hiU) | strB* }
//**
//** B = 8 bit, H = 16 bit, W = 32 bit, U = ULEB128 of W, U0/U1 = ULEB128 of W+1
//////////////////////////////////////////////////////////////////////////////

Const

  // Bytecode dump header.
  BCDUMP_HEAD1 = $1b;
  BCDUMP_HEAD2 = $4c;
  BCDUMP_HEAD3 = $4a;
  // If you perform *any* kind of private modifications to the bytecode itself
  // or to the dump format, you *must* set BCDUMP_VERSION to 0x80 or higher.
  BCDUMP_VERSION	=	1;

  // Compatibility flags.
  BCDUMP_F_BE	   = $01;
  BCDUMP_F_STRIP = $02;
  BCDUMP_F_FFI   = $04;
  BCDUMP_F_KNOWN = (BCDUMP_F_FFI*2-1);


  // Flags for prototype.
  PROTO_CHILD	       = $01;	// Has child prototypes.
  PROTO_VARARG       = $02;	// Vararg function.
  PROTO_FFI		       = $04;	// Uses BC_KCDATA for FFI datatypes.
  PROTO_NOJIT	       = $08;	// JIT disabled for this function.
  PROTO_ILOOP	       = $10;	// Patched bytecode with ILOOP etc.
  // Only used during parsing.
  PROTO_HAS_RETURN	 = $20;	// Already emitted a return.
  PROTO_FIXUP_RETURN = $40;	// Need to fixup emitted returns.
  // Top bits used for counting created closures.
  PROTO_CLCOUNT		   = $20;	// Base of saturating 3 bit counter.
  PROTO_CLC_BITS		 = 3;

  //////////////////////////////////////////////////////////////////////////////
Type
  PLuaItems = ^TLuaItems;
  TLuaItems = Object
  Public
    Flags    :Byte;
    NumParams:Byte;
    FrameSize:Byte;
    CodeData :TCodeData;
    ConstUV  :TConstUV;
    Resource :TResourceData;
    ConstNum :TConstNum;
    Constructor Create(ABuff:Pointer; ASize:LongWord; var AReturn:LongWord);
  End;
  TALuaItems = array[0..0] of TLuaItems;
  PALuaItems = ^TALuaItems;

  Constructor TLuaItems.Create(ABuff:Pointer; ASize:LongWord; var AReturn:LongWord);
  Var
    N : LongWord;
    fBuffData : PAnsiChar;
    fBuffSize : LongWord;
    Function Next(value:LongWord=1):Byte;
    Begin
      if value>fBuffSize then
      raise Exception.Create('TLuaItems.Next = error size');
      Inc(AReturn,value);
      Result := Byte(fBuffData^);
      Dec(fBuffSize,value);
      Inc(fBuffData,value);
    End;
    Function next_uleb128:LongWord;
    Var
      sh:Integer;
    Begin
      sh := 0;
      Result := 0;
      while Byte(fBuffData^) >= $80 do
      Begin
        Result := Result or ((next and $7f) shl sh);
        sh := sh + 7;
      End;
      Result := Result or ((next and $7f) shl sh);
    End;
  Begin
    AReturn   := 0;
    fBuffSize := ASize;
    fBuffData := ABuff;
    // Read prototype header.
    Flags     := Next;
    NumParams := Next;
    FrameSize := Next;
    ConstUV .Create(Next);
    Resource.CreateCHILD(next_uleb128);
    ConstNum.Create(next_uleb128);
    CodeData.Create(next_uleb128);
    N:=ReadCodeData(@CodeData,fBuffData,fBuffSize);
    Next(N);
    N:=ReadConstUV(@ConstUV,fBuffData,fBuffSize);
    Next(N);
    N:=ReadResource(@Resource,fBuffData,fBuffSize,true);
    Next(N);
    N:=ReadConstNum(@ConstNum,fBuffData,fBuffSize);
    Next(N);

  {
  /* Read prototype header. */
  flags     = bcread_byte(ls);
  numparams = bcread_byte(ls);
  framesize = bcread_byte(ls);
  sizeuv    = bcread_byte(ls);
  sizekgc   = bcread_uleb128(ls);
  sizekn    = bcread_uleb128(ls);
  sizebc    = bcread_uleb128(ls) + 1;

  // Read bytecode instructions and upvalue refs.
  bcread_bytecode(ls, pt, sizebc);
  bcread_uv(ls, pt, sizeuv);
  // Read constants.
  bcread_kgc(ls, pt, sizekgc);
  bcread_knum(ls, pt, sizekn);
  }
  End;
  //////////////////////////////////////////////////////////////////////////////
Type
  PLuaFile = ^TLuaFile;
  TLuaFile = Object
  Private
    FFlags:LongWord;
    FCount:LongWord;
    FItems:PLuaItems;
  Protected
    Function GetItems(index:Longword):PLuaItems;
  Public
    Constructor Create(ABuff:Pointer; ASize:LongWord; var AReturn:LongWord);
    property Count:LongWord read FCount;
    property Items[index:Longword]:PLuaItems read GetItems;// default;
  End;
  Function TLuaFile.GetItems(index:Longword):PLuaItems;
  Begin
    if index>=Count then
    raise Exception.Create('error TLuaFile.GetItems');
    Result := @PALuaItems(FItems)^[index];
  End;
  Constructor TLuaFile.Create(ABuff:Pointer; ASize:LongWord; var AReturn:LongWord);
  Var
    I, N, R : LongWord;
    fBuffData : PAnsiChar;
    fBuffSize : LongWord;
    Function Next(value:LongWord=1):Byte;
    Begin
      if value>fBuffSize then
      raise Exception.Create('TLuaFile.Next = error size');
      Inc(AReturn,value);
      Result := Byte(fBuffData^);
      Dec(fBuffSize,value);
      Inc(fBuffData,value);
    End;
    Function next_uleb128:LongWord;
    Var
      sh:Integer;
    Begin
      sh := 0;
      Result := 0;
      while Byte(fBuffData^) >= $80 do
      Begin
        Result := Result or ((next and $7f) shl sh);
        sh := sh + 7;
      End;
      Result := Result or ((next and $7f) shl sh);
    End;
  Begin
    FCount:=0;
    FItems:=nil;
    
    AReturn   := 0;
    fBuffSize := ASize;
    fBuffData := ABuff;

    //Проверка на тип файла
    if (Next<>BCDUMP_HEAD1)
    or (Next<>BCDUMP_HEAD2)
    or (Next<>BCDUMP_HEAD3)
    or (Next<>BCDUMP_VERSION) then
    raise Exception.Create('TLuaFile.Create = error file incorect');

    //Тип представляемых данных
    FFlags := next_uleb128;
    if (FFlags and (not BCDUMP_F_KNOWN))>0 then
    raise Exception.Create('TLuaFile.Create = error file BCDUMP_F_KNOWN');
    if (FFlags and BCDUMP_F_FFI)>0 then
    raise Exception.Create('TLuaFile.Create = error file BCDUMP_F_FFI');
    if (FFlags and BCDUMP_F_STRIP)=0 then
    raise Exception.Create('TLuaFile.Create = error file not BCDUMP_F_STRIP');

    //Подщитываем количество блоков
    N := next_uleb128;
    while N>0 do
    Begin
      Inc(FCount);
      Next(N);
      N := next_uleb128;
    End;
    GetMem(FItems,FCount*SizeOf(FItems^));

    AReturn   := 0;
    fBuffSize := ASize;
    fBuffData := ABuff;
    Next(5);

    //Загружаем подряд все блоки
    I := 0; N := next_uleb128;
    while N>0 do
    Begin
      Items[I].Create(fBuffData, fBuffSize, R);
      if R<>N then
      raise Exception.Create('TLuaFile.Items.Create = error file load');
      Next(N);
      N := next_uleb128;
      Inc(I);
    End;
  End;
  //////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
Function TextLuaFile(LuaFile:PLuaFile; Index:LongWord; TurnStep:LongWord=0;isClass:Boolean=false):AnsiString;
Var
  ResultStr:AnsiString absolute Result;

  I,J,N,K : Integer;
  tmpStr  : AnsiString;
  retStr  : AnsiString;

  code    : PBlockData;
  Next    : PBlockData;
  LuaInit : PLuaItems;

  FunCount:Integer;
  ArgInx:Integer;
  LenEnd:Integer;
  nenEnd:array[0..$ff] of integer;
  param:array[0..$ff,0..2] of AnsiString;

  nRead:LongWord;
  FHandleW:THandle;

  Procedure ResultAddLine(aValus:AnsiString;aTurn:Boolean=true);
  Begin
    if ResultStr<>'' then ResultStr := ResultStr + #13#10;
    if aTurn then ResultStr := ResultStr + Format('%-*s',[TurnStep,'']);
    ResultStr := ResultStr + aValus;
  End;

  Procedure ParamClear(index:Integer=-1);
  Begin
    if index>-1 then
    Begin
       param[index,0]:='';
       param[index,1]:='';
       param[index,2]:='';
    end else
    for index := 0 to $ff do
    Begin
      param[index,0]:='';
      param[index,1]:='';
      param[index,2]:='';
    end;
  End;
  Function ArgCreate:AnsiString;
  Begin
    Result := Format('arg_%.d',[ArgInx]);
    Inc(ArgInx);
  End;

  Function GetD(_itm : PBlockData=nil):Integer;
  var W:Word;
  Begin
    Result := 0;
    if _itm=nil then _itm := code;
    if _itm=nil then
    raise Exception.Create('VarToDestroy = error Code is null');
    W := PWord(@_itm^.ValuHi)^;
    if (W and $8000)<>0 then W := W and $7fff else W := W or $8000;
    Result := PSmallInt(@W)^;
  End;

  Function GetPrimitive(PriIndex:LongWord):AnsiString;
  Begin
    case PriIndex of
      0: Result := 'nil';
      1: Result := 'false';
      2: Result := 'true';
    else
      raise Exception.Create('TextLuaFile = error LuaFile is null');
    end;
  End;

  Function VarInArg(VarIndex:LongWord; Code:PCodeData; CodeIndex:LongWord; T:T_BCDEF_ARG_SET=[BCDEF_ARG_DST]):LongWord;
  Var I:Integer;
      O:PBlockData;
  Begin
    Result := 0;
    if Code=nil then
       raise Exception.Create('VarToDestroy = error Code is null');
    for I := CodeIndex+2 to Code^.Count do
    Begin
      O:=Code^.Items[I-1];
      if ((O^.FunRun=VarIndex) and (T_BCDEF_INFO[O^.OpCode].A in T))
      or ((O^.ValuLo=VarIndex) and (T_BCDEF_INFO[O^.OpCode].B in T))
      or ((O^.ValuHi=VarIndex) and (T_BCDEF_INFO[O^.OpCode].C in T)) Then
      Begin
        Result := I;
        Break;
      End;
    End;
  End;

  Function SetOperatorResult:boolean;
  var B:Longbool; S:AnsiString;
  Begin
    Result := False;
    // tmpStr - Текст операции и результат операции

    S := param[code.FunRun,0];

    B := (pos(' ',S)=0)and(pos('.',S)=0)and(pos('[',S)=0)and(pos('(',S)=0);

    if (S='') Then
    Begin
      param[code.FunRun,0] := ArgCreate;
      ResultAddLine('local ' + param[code.FunRun,0] + ' = ' +tmpStr);
      Result := True;
    End else
    if (Length(S)>4) and B and(S[1]='a')and(S[2]='r')and(S[3]='g')and(S[4]='_') then
    Begin
      ResultAddLine(param[code.FunRun,0] + ' = ' +tmpStr);
      Result := True;
    End else;
    if ((Length(S)>6) and B and(S[1]='p')and(S[2]='a')and(S[3]='r')and(S[4]='a')and(S[5]='m')and(S[6]='_')) then
    Begin
      ResultAddLine(param[code.FunRun,0] + ' = ' +tmpStr);
      Result := True;
    End else;
    exit;

    //_I := VarInArg(code.FunRun,@LuaInit^.CodeData,I,[BCDEF_ARG_DST]);
    //if _I>0 then
    {
    if (LuaInit^.NumParams>code.FunRun)and(code.FunRun<>code.ValuLo) then
    Begin
      ResultAddLine(param[code.FunRun,0] + ' = ' +tmpStr);
      Result := True;
    End;
    }
  End;

Begin
  Result := '';
  FunCount:=0;
  ArgInx:=0;
  ParamClear;
  LenEnd:=0;FillChar(nenEnd[0],SizeOf(nenEnd),0);

  if (LuaFile=nil)and(LuaFile^.Count>=Index) then
  raise Exception.Create('TextLuaFile = error LuaFile is null');

  LuaInit := LuaFile^.Items[Index];

  if LuaFile^.Count<>Index+1 then
  Begin
    if isClass then param[0,0] := 'self';
    retStr := '(';
    for I := 1+Integer(isClass) to LuaInit^.NumParams do
    Begin
      tmpStr := Format('param_%d',[I-1-Integer(isClass)]);
      param[I-1,0] := tmpStr;
      if retStr='(' then retStr := retStr + tmpStr else
      retStr := retStr+', '+tmpStr;
    End;
    retStr := retStr + ')';
    ResultAddLine(retStr,false);
  End;

  I := 0;
  while I<LuaInit^.CodeData.Count do
  Begin

    if I>0 then
    for J := 1 to LenEnd do
    if nenEnd[J-1]=I+1 then
    Begin
      TurnStep:=TurnStep-3;
      ResultAddLine('end');
    End else
    if -nenEnd[J-1]=I+1 then
    Begin
      ResultAddLine(Format('@@%.4d:',[-nenEnd[J-1]]));
    End;
    
    tmpStr:='';
    retStr:='';
    code := LuaInit^.CodeData.Items[i];
    if I+1>=LuaInit^.CodeData.Count then Next := nil else
    Next := LuaInit^.CodeData.Items[i+1];
    case code.OpCode of
     //-------------------------------------------------------------------------
     BCDEF_UCLO:
       Begin
         //'UCLO'  ,  'rbase',  '___',  'jump',  '___'
       End;
     BCDEF_TNEW:
       Begin
         //'TNEW'  ,  'dst'  ,  '___',  'lit' ,  'gc'
         tmpStr := ArgCreate;
         retStr := 'local ' + tmpStr + ' = {}';
         ResultAddLine(retStr);
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_FNEW:
       Begin
         //'FNEW'  ,  'dst'  ,  '___',  hi'func',  'gc'
         if ResultStr<>'' then ResultAddLine('');

         if Next.OpCode=BCDEF_TSETS then
         Begin
           retStr := 'function '+param[Next.ValuLo,0];
           tmpStr := LuaInit^.Resource.Items[Next.ValuHi].Value;

           if (code.ValuHi and 1)=0 then
           Begin
             retStr := retStr+'.'+tmpStr + TextLuaFile(LuaFile,{(code.ValuHi shr 1)-1}FunCount,TurnStep+2);
           End else
             retStr := retStr+'.'+tmpStr + TextLuaFile(LuaFile,{(code.ValuHi shr 1)-1}FunCount,TurnStep+2,tmpStr<>'create');
           retStr := retStr+#13#10+'end';
           Inc(FunCount);
           Inc(I);
         End else
         Begin
           tmpStr := Format('function_%d',[FunCount{code.ValuHi}]);
           param[code.ValuHi,0] := tmpStr;
           retStr := 'function ' + tmpStr + TextLuaFile(LuaFile, FunCount{code.ValuHi} ,TurnStep+2);
           retStr := retStr + #13#10+'end';
           Inc(FunCount);
         End;

         ResultAddLine(retStr);


         FHandleW:=CreateFile('demo.txt',GENERIC_WRITE,FILE_SHARE_WRITE,NIL,CREATE_ALWAYS,0,0);
         if FHandleW<>INVALID_HANDLE_VALUE Then
         Try
           WriteFile(FHandleW, retStr[1], Length(retStr), nRead, nil);
         finally
           CloseHandle(FHandleW);FHandleW:=0;
         end;

       End;
     //-------------------------------------------------------------------------
     BCDEF_KPRI:
       Begin
         //'KPRI'  ,  'dst'  ,  '___',  'pri' ,  '___'
         tmpStr := GetPrimitive(code.ValuHi);
         if not SetOperatorResult Then
         Begin
           tmpStr := ArgCreate;
           ResultAddLine('local ' + tmpStr + ' = ' + GetPrimitive(code.ValuHi));
           param[code.FunRun,0] := tmpStr;
         End;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_KSHORT:
       Begin
         //'KSHORT',  'dst'  ,  '___',  'lits',  '___'
         tmpStr := Format('%.d',[PSmallInt(@code.ValuHi)^]);
         if not SetOperatorResult Then
         Begin
            tmpStr := ArgCreate;
            ResultAddLine('local ' + tmpStr + ' = ' + Format('%.d',[PSmallInt(@code.ValuHi)^]));
            param[code.FunRun,0]:= tmpStr;
         End;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_KNUM:
       Begin
         //'KNUM'  ,  'dst'  ,  '___',  'num' ,  '___'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         if not SetOperatorResult Then
         Begin
           tmpStr := ArgCreate;
           ResultAddLine('local ' + tmpStr + ' = ' + LuaInit^.ConstNum.Items[code.ValuHi]^);
           param[code.FunRun,0]:= tmpStr;
         End;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_KSTR:
       Begin
         //'KSTR'  ,  'dst'  ,  '___',  'str' ,  '___'
         tmpStr := '"' + LuaInit^.Resource.Items[code.ValuHi]^.Value + '"';
         if not SetOperatorResult Then
         Begin
           tmpStr := ArgCreate;
           ResultAddLine('local ' + tmpStr + ' = ' + '"' + LuaInit^.Resource.Items[code.ValuHi]^.Value + '"');
           param[code.FunRun,0]:= tmpStr;
         End;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_UNM:
       Begin
         //dst, ___, var, unm
         tmpStr := ' - ' + param[code.ValuHi,0];
         if not SetOperatorResult Then
         Begin
           tmpStr := ArgCreate;
           ResultAddLine('local ' + tmpStr + ' = ' + ' - '+ param[code.ValuHi,0]);
           param[code.FunRun,0]:= tmpStr;
         End;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     //-------------------------------------------------------------------------
     BCDEF_UGET:
       Begin
         //UGET, dst, __, uv, __
         tmpStr := Format('%d',[LuaInit^.ConstUV.Items[code.ValuHi]^]);
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_GSET:
       Begin
         //'GSET'  ,  'var'  ,  '___',  'str' ,  'newindex'
         tmpStr := LuaInit^.Resource.Items[code.ValuHi].Value;
         retStr := tmpStr + ' = ' + param[code.FunRun,0];
         ResultAddLine(retStr);
         param[code.FunRun,0]:= retStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_GGET:
       Begin
         //'GGET'  ,  'dst'  ,  '___',  'str' ,  'index'
         tmpStr := LuaInit^.Resource.Items[code.ValuHi].Value;
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_TGETV:
       Begin
         //'TGETV' ,  'dst'  ,  'var',  'var' ,  'index'
         tmpStr := param[code.ValuLo,0]+'['+param[code.ValuHi,0] + ']';
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_TSETV:
       Begin
         //'TSETV' ,  'var'  ,  Lo'var',  hi'var' ,  'newindex'
         retStr := param[code.Valulo,0] + '[' + param[code.ValuHi,0] + '] = ';
         if param[code.FunRun,0]='' Then retStr := retStr + '0' else
         retStr := retStr + param[code.FunRun,0];
         ResultAddLine(retStr);
       End;
     BCDEF_TGETS:
       Begin
         //'TGETS' ,  'dst'  ,  'var',  'str' ,  'index'
         tmpStr := LuaInit^.Resource.Items[code.ValuHi].Value;
         param[code.FunRun,0]:= param[code.ValuLo,0]+'.'+tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_TSETS:
       Begin
         //'TSETS' ,  'var'  ,  Lo'var',  hi'str' ,  'newindex'
         tmpStr := LuaInit^.Resource.Items[code.ValuHi].Value;
         retStr := param[code.Valulo,0];
         //Если начинается с большой буквы или есть пробелы значит это текст
         if (tmpStr='')or(tmpStr[1] in ['A'..'Z'])or(pos(' ',tmpStr)>0) then retStr := retStr + '["' + tmpStr + '"] = ' else
         retStr := retStr + '.' + tmpStr + ' = ';
         if param[code.FunRun,0]='' Then retStr := retStr + '0' else
         retStr := retStr + param[code.FunRun,0];
         ResultAddLine(retStr);
       End;
     BCDEF_TSETB:
       Begin
         //TSETB, var, var, lit, newindex
         //R(B)[C] = R(A)
         retStr := Format('%s[%d] = %s',[param[code.Valulo,0],code.ValuHi,param[code.FunRun,0]]);
         ResultAddLine(retStr);
       End;
     BCDEF_TGETB:
       Begin
         //TGETB, dst, var, lit, index
         //R(A) = R(B)[C]
         tmpStr := Format('%s[%d]',[param[code.Valulo,0],code.ValuHi]);
         if not SetOperatorResult then
         Begin
           tmpStr := ArgCreate;
           ResultAddLine('local ' + tmpStr + ' = ' + Format('%s[%d]',[param[code.Valulo,0],code.ValuHi]));
           param[code.FunRun,0]:= tmpStr;
         End;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     //-------------------------------------------------------------------------
     BCDEF_TDUP:
       Begin
         //'TDUP', 'dst', '___', 'tab', 'gc'
         tmpStr := '{' + TextResource(LuaInit^.Resource.Items[code.ValuHi]) + '}';
         if not SetOperatorResult then
         Begin
           tmpStr := ArgCreate;
           ResultAddLine('local ' + tmpStr + ' = ' + '{' + TextResource(LuaInit^.Resource.Items[code.ValuHi]) + '}');
           param[code.FunRun,0]:= tmpStr;
         End;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_VARG:
       Begin
         //VARG, base, lit, lit, ___
         param[code.FunRun,0]:= '...';
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_MOV:
       Begin
         //'MOV'   ,  'dst',  '___',  'var',  '___'
         param[code.FunRun,0]:= param[code.ValuHi,0];
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     //-------------------------------------------------------------------------
     BCDEF_LEN:
       Begin
         //'LEN'   ,  'dst',  '___',  'var',  'len'
         tmpStr := '#' + param[code.ValuHi,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_NOT:
       Begin
         //'NOT', 'dst', '__', 'var', '__'
         tmpStr := 'not ' + param[code.ValuHi,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_CAT:
       Begin
         //'CAT', 'dst', 'rbase', 'rbase',,'concat'
         tmpStr := param[code.ValuLo,0] + ' .. ' + param[code.ValuHi,0];
         if not SetOperatorResult then
         Begin
           tmpStr := ArgCreate;
           ResultAddLine('local ' + tmpStr + ' = ' + param[code.ValuLo,0] + ' .. ' + param[code.ValuHi,0]);
           param[code.FunRun,0]:= tmpStr;
         End;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     //-------------------------------------------------------------------------
     BCDEF_SUBVV:
       Begin
         //'SUBVV' ,  'dst',  'var',  'var',  'sub'
         tmpStr := param[code.ValuLo,0]+' - '+param[code.ValuHi,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_ADDVV:
       Begin
         //'ADDVV' ,  'dst',  'var',  'var',  'add'
         tmpStr := param[code.ValuLo,0]+' + '+param[code.ValuHi,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_MULVV:
       Begin
         //'MULVV' ,  'dst',  'var',  'var',  'mul'
         tmpStr := param[code.ValuLo,0]+' * '+param[code.ValuHi,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_DIVVV:
       Begin
         //'DIVVV' ,  'dst',  'var',  'var',  'div'
         tmpStr := param[code.ValuLo,0]+' / '+param[code.ValuHi,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_MODVV:
       Begin
         //'MODVV' ,  'dst',  'var',  'var',  'mod'
         tmpStr := param[code.ValuLo,0]+' % '+param[code.ValuHi,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     //-------------------------------------------------------------------------
     BCDEF_ADDVN:
       Begin
         //'ADDVN' ,  'dst',  'var',  'num',  'add'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         tmpStr := param[code.ValuLo,0] + ' + ' + tmpStr;
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_SUBVN:
       Begin
         //'SUBVN' ,  'dst',  'var',  'num',  'sub'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         tmpStr := param[code.ValuLo,0] + ' - ' + tmpStr;
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_MULVN:
       Begin
         //'MULVN' ,  'dst',  'var',  'num',  'mul'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         tmpStr := param[code.ValuLo,0] + ' * ' + tmpStr;
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_DIVVN:
       Begin
         //'DIVVN' ,  'dst',  'var',  'num',  'div'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         tmpStr := param[code.ValuLo,0] + ' / ' + tmpStr;
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_MODVN:
       Begin
         //'MODVN' ,  'dst',  'var',  'num',  'mod'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         tmpStr := param[code.ValuLo,0] + ' % ' + tmpStr;
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     //-------------------------------------------------------------------------
     BCDEF_ADDNV:
       Begin
         //'ADDNV' ,  'dst',  'var',  'num',  'add'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         tmpStr := tmpStr + ' + ' + param[code.ValuLo,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_SUBNV:
       Begin
         //'SUBNV' ,  'dst',  'var',  'num',  'sub'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         tmpStr := tmpStr + ' - ' + param[code.ValuLo,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_MULNV:
       Begin
         //'MULNV' ,  'dst',  'var',  'num',  'mul'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         tmpStr := tmpStr + ' * ' + param[code.ValuLo,0];
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_DIVNV:
       Begin
         //'DIVNV' ,  'dst',  'var',  'num',  'div'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         tmpStr := tmpStr + ' / ' + param[code.ValuLo,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_MODNV:
       Begin
         //'MODNV' ,  'dst',  'var',  'num',  'mod'
         tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
         tmpStr := tmpStr + ' % ' + param[code.ValuLo,0];
         if not SetOperatorResult then
         param[code.FunRun,0]:= tmpStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     //-------------------------------------------------------------------------
     BCDEF_IST:
       Begin
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           retStr := 'if not ' + param[code.ValuHi,0] + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISF:
       Begin
         //'ISF'   ,  '___',  '___',  'var',  '___'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           retStr := 'if ' + param[code.ValuHi,0] + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISFC:
       Begin
         //ISFC, dst, __, var, __
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           tmpStr := ArgCreate;
           ResultAddLine('local ' + tmpStr + ' = ' +param[code.ValuHi,0]+ ' == false');
           ResultAddLine('if ' + tmpStr + ' then');
           param[code.FunRun,0] := tmpStr;
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISGT:
       Begin
         //'ISGT'  ,  'var',  '___',  'var',  'le'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           retStr := 'if ' + param[code.FunRun,0] + ' <= ' + param[code.ValuHi,0] + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISLT:
       Begin
         //'ISLT'  ,  'var',  '___',  'var',  'le'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           retStr := 'if ' + param[code.FunRun,0] + ' >= ' + param[code.ValuHi,0] + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISGE:
       Begin
         //'ISGE'  ,  'var',  '___',  'var',  'le'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           retStr := 'if ' + param[code.FunRun,0] + ' < ' + param[code.ValuHi,0] + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISLE:
       Begin
         //'ISLE'  ,  'var',  '___',  'var',  'le'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           retStr := 'if ' + param[code.FunRun,0] + ' > ' + param[code.ValuHi,0] + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISNES:
       Begin
         //'ISNES' ,  'var',  '___',  'str',  'eq'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           tmpStr := LuaInit^.Resource.Items[code.ValuHi].Value;
           retStr := 'if ' + param[code.FunRun,0] + ' == "' + tmpStr + '" then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISEQS:
       Begin
         //'ISEQS' ,  'var',  '___',  'str',  'eq'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           tmpStr := LuaInit^.Resource.Items[code.ValuHi].Value;
           retStr := 'if ' + param[code.FunRun,0] + ' != "' + tmpStr + '" then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISNEV:
       Begin
         //'ISNEV' ,  'var',  '___',  'var',  'eq'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           retStr := 'if ' + param[code.FunRun,0] + ' == ' + param[code.ValuHi,0] + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISEQV:
       Begin
         //'ISEQV' ,  'var',  '___',  'var',  'eq'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           if param[code.FunRun,0]='' then retStr := 'if 0 != ' else
           retStr := 'if ' + param[code.FunRun,0] + ' != ';
           retStr := retStr + param[code.ValuHi,0] + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISNEN:
       Begin
         //'ISNEN' ,  'var',  '___',  'num',  'eq'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
           retStr := 'if ' + param[code.FunRun,0] + ' == ' + tmpStr + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISEQN:
       Begin
         //'ISEQN' ,  'var',  '___',  'num',  'eq'
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           tmpStr := LuaInit^.ConstNum.Items[code.ValuHi]^;
           retStr := 'if ' + param[code.FunRun,0] + ' != ' + tmpStr + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISNEP:
       Begin
         //'ISNEP' ,  'var',  '___',  'pri',  'eq')
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           tmpStr := GetPrimitive(code.ValuHi);
           retStr := 'if ' + param[code.FunRun,0] + ' == ' + tmpStr + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ISEQP:
       Begin
         //'ISEQP' ,  'var',  '___',  'pri',  'eq')
         if Next.OpCode=BCDEF_JMP then
         Begin
           nenEnd[LenEnd]:=I+3+Next.ValuHi;Inc(LenEnd);
           tmpStr := GetPrimitive(code.ValuHi);
           retStr := 'if ' + param[code.FunRun,0] + ' != ' + tmpStr + ' then';
           ResultAddLine(retStr);
           TurnStep:=TurnStep+3;
           Inc(I);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     //-------------------------------------------------------------------------
     BCDEF_JMP:
       Begin
         //'JMP'   ,  'rbase',  '___',  'jump',  '___'
         nenEnd[LenEnd]:=-(I+2+code.ValuHi);Inc(LenEnd);
         retStr := Format('goto @@%.4d',[I+2+code.ValuHi]);
         ResultAddLine(retStr)
       End;
     //-------------------------------------------------------------------------
     BCDEF_ISNEXT:
       Begin
         //ISNEXT, base, __, jump, __
         J := GetD()+1;
         IF (PABlockData(code)[J+0].OpCode in [BCDEF_ITERN,BCDEF_ITERC])
         or (PABlockData(code)[J+1].OpCode in [BCDEF_ITERL]) Then
         Begin
           //Запоминаем что надо проставить текст выхода из цыкла и увеличиваем отступ
           nenEnd[LenEnd]:=-(I+1+J);Inc(LenEnd);
           //Проверяем указатель взврата в цыкл
           N := J + 1 + GetD(@PABlockData(code)[J+1]);
           //Запрашиваем количество участвующих в цикле переменных
           N := PABlockData(code)[J].ValuHi;

           param[code.FunRun,0] := ArgCreate;
           retStr := 'for ' + param[code.FunRun,0] + ' = ';
           if N>0 then
           Begin
              K := code.FunRun-N+1;
              while K<code.FunRun do
              Begin
                if K=code.FunRun-N+1 then retStr := retStr + param[K,0] else
                retStr := retStr+', '+param[K,0];
                inc(K);
              End;
           End else
           raise Exception.Create('TextLuaFile = error LuaFile is BCDEF_ISNEXT');

           retStr := retStr + ' do ';
           ResultAddLine(retStr);
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is null');
       End;
     BCDEF_ITERL:
       Begin
         //ITERL, base, __, jump, __
         J := GetD(code);
         IF PABlockData(code)[J].OpCode in [BCDEF_ISNEXT,BCDEF_JMP] Then
         Begin
           //Возможно поже обработаю по другому
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is BCDEF_ITERL');
       End;
     BCDEF_ITERN,BCDEF_ITERC:
       Begin
         //ITERN, base, lit, lit, call
         if next^.OpCode = BCDEF_ITERL then
         Begin
           J := GetD(next);
           IF PABlockData(code)[J+1].OpCode in [BCDEF_ISNEXT,BCDEF_JMP] Then
           Begin
             //Возможно поже обработаю по другому
           End else
           raise Exception.Create('TextLuaFile = error LuaFile is BCDEF_ITERN');
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is BCDEF_ITERN');
       End;
     {
     BCDEF_ITERC:
       Begin
         //ITERN, base, lit, lit, call
         raise Exception.Create('TextLuaFile = error LuaFile is BCDEF_ITERC');
       End;
     }
     //-------------------------------------------------------------------------
     BCDEF_FORI:
       Begin
         //'FORI'  ,  'base' ,  '___',  'jump',  '___'
         param[code^.FunRun + 3,0] := ArgCreate;
         retStr := 'for ' + param[code^.FunRun + 3,0] + ' = ';
         J := code.FunRun;
         while J<code.FunRun+3 do
         Begin
           if J=code.FunRun then retStr := retStr + param[J,0] else
           retStr := retStr+', '+param[J,0];
           inc(J);
         End;
         retStr := retStr + ' do';
         ResultAddLine(retStr);
         TurnStep:=TurnStep+3;
         J := PSmallInt(@code.ValuHi)^ AND $3fff;
         nenEnd[LenEnd]:=I+2+J;Inc(LenEnd);
       End;
     BCDEF_FORL:
       Begin
         //'FORL'  ,  'base' ,  '___',  'jump',  '___'
         //retStr := Format('-- forL %.d, %.d',[code.FunRun,PSmallInt(@code.ValuHi)^]);
         //ResultAddLine(retStr);
       End;
     //-------------------------------------------------------------------------
     BCDEF_RET:
       Begin
         //'RET'   ,  'rbase',  '___',  'lit' ,  '___'
         retStr := 'return';
         J := LuaInit^.NumParams;
         while J<LuaInit^.NumParams+code.ValuHi do
         Begin
           if J=LuaInit^.NumParams then retStr := retStr + ' ' +  param[J,0] else
           retStr := retStr+', '+param[J,0];
           inc(J);
         End;
         ResultAddLine(retStr);
       End;
     BCDEF_RET0:
       Begin
         //'RET0'  ,  'rbase',  '___',  'lit' ,  '___'
         if I+1 < LuaInit.CodeData.Count Then
         Begin
           retStr := 'return';
           ResultAddLine(retStr);
         End;
       End;
     BCDEF_RET1:
       Begin
         //'RET1'  ,  'rbase',  '___',  'lit' ,  '___'
         retStr := 'return ' + param[code.FunRun,0];
         ResultAddLine(retStr);
       End;
     BCDEF_RETM:
       Begin
         //RETM, base, __, lit, __
         //R(A)..R(A+D-1)
         retStr := 'return';
         J := code^.FunRun;
         while J<code^.FunRun+code^.ValuHi-1 do
         Begin
           if J=code^.FunRun+code^.ValuHi-1 then retStr := retStr + ' ' +  param[J,0] else
           retStr := retStr+', '+param[J,0];
           inc(J);
         End;
         ResultAddLine(retStr);
       End;
     //-------------------------------------------------------------------------
     BCDEF_CALLM:
       Begin
         //'CALLM' ,  'base' ,  'lit',  'lit' ,  'call'
         retStr := param[code.FunRun,0]+'(';
         J := code.FunRun+1;
         while J<code.FunRun+1+code.ValuLo+code.ValuHi do
         Begin
           if J=code.FunRun+1 then retStr := retStr + param[J,0] else
           retStr := retStr+', '+param[J,0];
           inc(J);
         End;
         retStr := retStr + ')';
         if not(next.OpCode in[BCDEF_ISLT..BCDEF_ISF]) then
         ResultAddLine(retStr);
         param[code.FunRun,0]:= retStr;;
         param[code.FunRun,1]:= '';;
         param[code.FunRun,2]:= '';
       End;
     BCDEF_CALLMT:
       Begin
         //CALLMT, base, ___, lit, call
         retStr := param[code.FunRun,0]+'(';
         J := code.FunRun+1;
         while J<=code.FunRun+code.ValuHi+1 do
         Begin
           if J=code.FunRun+1 then retStr := retStr + param[J,0] else
           retStr := retStr+', '+param[J,0];
           inc(J);
         End;
         retStr := retStr + ')';
         ResultAddLine(retStr);
         param[code.FunRun,0]:= retStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_CALLT:
       Begin
         //'CALLT' ,  'base' ,  '___',  'lit' ,  'call'
         retStr := param[code.FunRun,0]+'(';
         J := code.FunRun+1;
         while J<code.FunRun+code.ValuHi do
         Begin
           if J=code.FunRun+1 then retStr := retStr + param[J,0] else
           retStr := retStr+', '+param[J,0];
           inc(J);
         End;
         retStr := retStr + ')';
         ResultAddLine(retStr);
         param[code.FunRun,0]:= retStr;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     BCDEF_CALL:
       Begin
         //'CALL'  ,  'base' ,  'lit',  'lit' ,  'call'
         //call 'FunRun' with 'ValuHi' arguments and 'ValuLo' result
         retStr := param[code.FunRun,0];

         tmpStr := '';
         if code.ValuHi>0 then
         Begin
           J := code.FunRun+1;
           while J<code.FunRun+code.ValuHi do
           Begin
             if J=code.FunRun+1 then tmpStr := tmpStr + param[J,0] else
             tmpStr := tmpStr+', '+param[J,0];
             inc(J);
           End;
           tmpStr := '(' + tmpStr + ')';
         End else
         raise Exception.Create('TextLuaFile = error LuaFile is BCDEF_CALL arguments count<0');

         retStr := retStr + tmpStr;
         {
         J := VarInArg(code.FunRun,@LuaInit^.CodeData,I,[BCDEF_ARG_DST]);
         N := VarInArg(code.FunRun,@LuaInit^.CodeData,I,[BCDEF_ARG_VAR]);
         if (N=0)or(N>J) then
            param[code.FunRun,0] := retStr;
         }

         case next.OpCode of
           BCDEF_ISLT..BCDEF_ISF:
             param[code.FunRun,0] := retStr;
           BCDEF_GSET:
             param[code.FunRun,0] := retStr;
         else
           tmpStr := '';
           if code.ValuLo>0 then
           Begin
             J := 1;
             while J<code.ValuLo do
             Begin
               param[code.FunRun+J-1,0] := ArgCreate;
               param[code.FunRun+J-1,1]:= '';
               param[code.FunRun+J-1,2]:= '';
               if J=1 then tmpStr := tmpStr + param[code.FunRun+J-1,0] else
               tmpStr := tmpStr + ', ' + param[code.FunRun+J-1,0];
               inc(J);
             End;
             //param[code.FunRun,0] := tmpStr;
             if tmpStr<>'' Then
             retStr := 'local ' + tmpStr + ' = ' + retStr;
             ResultAddLine(retStr);
           End else
           Begin
             //Если вся функция это входной параметер для следующей функции 
             param[code.FunRun,0] := retStr;
             ResultAddLine('--'+retStr);
           End;
         end;
         param[code.FunRun,1]:= '';
         param[code.FunRun,2]:= '';
       End;
     //-------------------------------------------------------------------------
    {
    BCDEF_ISTC  ,  BCDEF_POW   ,

    BCDEF_KCDATA,  
    BCDEF_KNIL  ,

    BCDEF_USETV ,  BCDEF_USETS ,  BCDEF_USETN ,  BCDEF_USETP ,

    BCDEF_TSETM ,


    BCDEF_JFORI ,  BCDEF_IFORL ,  BCDEF_JFORL ,
    BCDEF_IITERL,  BCDEF_JITERL,
    BCDEF_LOOP  ,  BCDEF_ILOOP ,  BCDEF_JLOOP ,


    BCDEF_FUNCF ,  BCDEF_IFUNCF,  BCDEF_JFUNCF,
    BCDEF_FUNCV ,  BCDEF_IFUNCV,  BCDEF_JFUNCV,  BCDEF_FUNCC ,
    BCDEF_FUNCCW:
    }
    else
      retStr := '--'+T_BCDEF_INFO[code.OpCode].OP +', '+ param[code.FunRun,0]+', '+ param[code.ValuLo,0]+', '+ param[code.ValuHi,0];
      //if T_BCDEF_INFO[code.OpCode].A in [BCDEF_ARG_VAR, BCDEF_ARG_VAR] then
      
      ResultAddLine(retStr);

      //raise Exception.Create('TextLuaFile = error LuaFile is null');
    end;
    Inc(I);
  End;


End;
////////////////////////////////////////////////////////////////////////////////


  Function next_uleb128_33(Values:PAnsiChar):LongWord;
  Var
    sh:Integer;
    fBuffData : PAnsiChar;
    Function Next(value:LongWord=1):Byte;
    Begin
      Result := Byte(fBuffData^);
      Inc(fBuffData,1);
    End;
  Begin
    sh := 0;
    Result := 0;
    fBuffData := Values;
    if (Byte(fBuffData^) and $40)<>0 then
    Begin
      Result := next and $3f;
      sh := 6;
      while (Byte(fBuffData^) and $80)<>0 do
      Begin
        Result := Result or ((next and $7f) shl sh);
        sh := sh + 7;
      End;
      Result := Result or (next shl sh);
    End else
    Result := next;
  End;


Var
    FHandle : THandle;
    fData : Pointer;
    fSize : LongWord;
    nRead : DWORD;

    I:Integer;
    tmp:AnsiString;
    FHandleW : THandle;

    CodeData:TCodeData;
    Resource:TResourceData;
    LuaFile:TLuaFile;
    fname:AnsiString;

Begin
  next_uleb128_33(#$6F#$D1#$B6#$7D);


  fname := 'CharSheet'; //4
  //fname := 'monsters';
  //fname := 'recipes';
  //fname := 'skills'; //8
  //fname := 'spells'; //8
  //fname := 'samples';
  //fname := 'talents';
  //fname := 'Config'; //4
  //fname := 'Console';
  //fname := 'tutorial';
  fname := 'MainMenu';
  //fname := 'items';
  fname := 'ToolTip';
  //fname := 'Array';
  //fname := 'NewGameMenu';
  //fname := 'MapEditor';
  //fname := 'Arch';
  //fname := 'CharClass';
  fname := 'Dream';
  fname := 'level07';
  fname := 'item';
  fname := 'Map';
  fname := 'Grimrock';
  fname := 'Spawner';
  fname := 'BaseEntity';


  FHandle:=CreateFile(PAnsiChar(fname+'_114.lua'), GENERIC_READ, FILE_SHARE_READ, NIL, OPEN_EXISTING, 0, 0);
  if FHandle<>INVALID_HANDLE_VALUE Then
  try
      fSize := SetFilePointer(FHandle, 0, nil, FILE_END);
      SetFilePointer(FHandle, 0, nil, FILE_BEGIN);
      GetMem(fData,fSize);
      Try
        if not ReadFile(FHandle ,fData^, fSize, nRead, nil) or (nRead<>fSize) Then
        raise Exception.Create('error ReadFile');

        LuaFile.Create(fData, fSize, nRead);
        if nRead<>fSize then
        raise Exception.Create('error LuaFile.Create');
        tmp := TextLuaFile(@LuaFile,LuaFile.Count-1);
        if tmp='' then
        raise Exception.Create('error TextLuaFile');
        {
        tmp := '';
        for I := 1 to LuaFile.Count do
        Begin
          if tmp<>'' then tmp := tmp + #13#10#13#10;
          tmp := tmp + TextResource(@LuaFile.Items[i-1]^.Resource,0,True);
        End;
        FHandleW:=CreateFile(PAnsiChar(fname+'_114_res.lua'),GENERIC_WRITE,FILE_SHARE_WRITE,NIL,CREATE_ALWAYS,0,0);
        }
        FHandleW:=CreateFile(PAnsiChar(fname+'_114.txt'),GENERIC_WRITE,FILE_SHARE_WRITE,NIL,CREATE_ALWAYS,0,0);
        if FHandleW<>INVALID_HANDLE_VALUE Then
        Try
          WriteFile(FHandleW, tmp[1], Length(tmp), nRead, nil);
        finally
          CloseHandle(FHandleW);
        end;

      Finally
        FreeMem(fData);
      End;
  finally
    CloseHandle(FHandle);
  end;

End.
