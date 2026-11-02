Attribute VB_Name = "VbaNullstr"
Option Explicit

Private Type TS
    a As String
    b As String
End Type

Private ModNever As String
Private ModEmpty As String
Private ModArr(1 To 2) As String
Private Const CEmpty As String = ""

Private Function RetNever() As String
End Function

Private Function RetEmpty() As String
    RetEmpty = ""
End Function

Private Function RetNullStr() As String
    RetNullStr = vbNullString
End Function

Private Function RetParam(ByVal s As String) As String
    RetParam = s
End Function

Private Function RetVariant() As Variant
    Dim s As String
    RetVariant = s
End Function

Private Function ByRefPass(ByRef s As String) As Boolean
    ByRefPass = (StrPtr(s) <> 0)
End Function

Private Function ByValPass(ByVal s As String) As Boolean
    ByValPass = (StrPtr(s) <> 0)
End Function

Private Function VarPass(ByVal v As Variant) As Boolean
    VarPass = (StrPtr(v) <> 0)
End Function

Public Function Np_LocalNever() As Variant
    On Error GoTo EH
    Dim s As String
    Np_LocalNever = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_LocalNever = "#ERR " & Err.Number
End Function

Public Function Np_LocalEmpty() As Variant
    On Error GoTo EH
    Dim s As String
    s = ""
    Np_LocalEmpty = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_LocalEmpty = "#ERR " & Err.Number
End Function

Public Function Np_LocalNullStr() As Variant
    On Error GoTo EH
    Dim s As String
    s = vbNullString
    Np_LocalNullStr = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_LocalNullStr = "#ERR " & Err.Number
End Function

Public Function Np_LocalEmptyThenNullStr() As Variant
    On Error GoTo EH
    Dim s As String
    s = "": s = vbNullString
    Np_LocalEmptyThenNullStr = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_LocalEmptyThenNullStr = "#ERR " & Err.Number
End Function

Public Function Np_LocalTextThenEmpty() As Variant
    On Error GoTo EH
    Dim s As String
    s = "x": s = ""
    Np_LocalTextThenEmpty = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_LocalTextThenEmpty = "#ERR " & Err.Number
End Function

Public Function Np_LocalTextThenNullStr() As Variant
    On Error GoTo EH
    Dim s As String
    s = "x": s = vbNullString
    Np_LocalTextThenNullStr = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_LocalTextThenNullStr = "#ERR " & Err.Number
End Function

Public Function Np_LocalCopyOfNever() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    t = s
    Np_LocalCopyOfNever = (StrPtr(t) <> 0)
    Exit Function
EH:
    Np_LocalCopyOfNever = "#ERR " & Err.Number
End Function

Public Function Np_LocalCopyOfEmpty() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    s = "": t = s
    Np_LocalCopyOfEmpty = (StrPtr(t) <> 0)
    Exit Function
EH:
    Np_LocalCopyOfEmpty = "#ERR " & Err.Number
End Function

Public Function Np_LocalCopyViaTextThenCopy() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    t = "x": t = s
    Np_LocalCopyViaTextThenCopy = (StrPtr(t) <> 0)
    Exit Function
EH:
    Np_LocalCopyViaTextThenCopy = "#ERR " & Err.Number
End Function

Public Function Np_ModNever() As Variant
    On Error GoTo EH
    Np_ModNever = (StrPtr(ModNever) <> 0)
    Exit Function
EH:
    Np_ModNever = "#ERR " & Err.Number
End Function

Public Function Np_ModEmpty() As Variant
    On Error GoTo EH
    ModEmpty = ""
    Np_ModEmpty = (StrPtr(ModEmpty) <> 0)
    Exit Function
EH:
    Np_ModEmpty = "#ERR " & Err.Number
End Function

Public Function Np_ModArrNever() As Variant
    On Error GoTo EH
    Np_ModArrNever = (StrPtr(ModArr(1)) <> 0)
    Exit Function
EH:
    Np_ModArrNever = "#ERR " & Err.Number
End Function

Public Function Np_ModArrEmpty() As Variant
    On Error GoTo EH
    ModArr(2) = ""
    Np_ModArrEmpty = (StrPtr(ModArr(2)) <> 0)
    Exit Function
EH:
    Np_ModArrEmpty = "#ERR " & Err.Number
End Function

Public Function Np_LocalArrNever() As Variant
    On Error GoTo EH
    Dim a(1 To 2) As String
    Np_LocalArrNever = (StrPtr(a(1)) <> 0)
    Exit Function
EH:
    Np_LocalArrNever = "#ERR " & Err.Number
End Function

Public Function Np_LocalArrEmpty() As Variant
    On Error GoTo EH
    Dim a(1 To 2) As String
    a(1) = ""
    Np_LocalArrEmpty = (StrPtr(a(1)) <> 0)
    Exit Function
EH:
    Np_LocalArrEmpty = "#ERR " & Err.Number
End Function

Public Function Np_LocalDynArr() As Variant
    On Error GoTo EH
    Dim a() As String
    ReDim a(1 To 2)
    Np_LocalDynArr = (StrPtr(a(1)) <> 0)
    Exit Function
EH:
    Np_LocalDynArr = "#ERR " & Err.Number
End Function

Public Function Np_LocalDynArrPreserve() As Variant
    On Error GoTo EH
    Dim a() As String
    ReDim a(1 To 1): ReDim Preserve a(1 To 2)
    Np_LocalDynArrPreserve = (StrPtr(a(1)) <> 0)
    Exit Function
EH:
    Np_LocalDynArrPreserve = "#ERR " & Err.Number
End Function

Public Function Np_UdtNever() As Variant
    On Error GoTo EH
    Dim u As TS
    Np_UdtNever = (StrPtr(u.a) <> 0)
    Exit Function
EH:
    Np_UdtNever = "#ERR " & Err.Number
End Function

Public Function Np_UdtEmpty() As Variant
    On Error GoTo EH
    Dim u As TS
    u.a = ""
    Np_UdtEmpty = (StrPtr(u.a) <> 0)
    Exit Function
EH:
    Np_UdtEmpty = "#ERR " & Err.Number
End Function

Public Function Np_UdtCopy() As Variant
    On Error GoTo EH
    Dim u As TS, w As TS
    w = u
    Np_UdtCopy = (StrPtr(w.a) <> 0)
    Exit Function
EH:
    Np_UdtCopy = "#ERR " & Err.Number
End Function

Public Function Np_StaticNever() As Variant
    On Error GoTo EH
    Static s As String
    Np_StaticNever = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_StaticNever = "#ERR " & Err.Number
End Function

Public Function Np_ConstEmpty() As Variant
    On Error GoTo EH
    Np_ConstEmpty = (StrPtr(CEmpty) <> 0)
    Exit Function
EH:
    Np_ConstEmpty = "#ERR " & Err.Number
End Function

Public Function Np_ConstLocalEmpty() As Variant
    On Error GoTo EH
    Const c As String = ""
    Np_ConstLocalEmpty = (StrPtr(c) <> 0)
    Exit Function
EH:
    Np_ConstLocalEmpty = "#ERR " & Err.Number
End Function

Public Function Np_RetNever() As Variant
    On Error GoTo EH
    Np_RetNever = (StrPtr(RetNever()) <> 0)
    Exit Function
EH:
    Np_RetNever = "#ERR " & Err.Number
End Function

Public Function Np_RetEmpty() As Variant
    On Error GoTo EH
    Np_RetEmpty = (StrPtr(RetEmpty()) <> 0)
    Exit Function
EH:
    Np_RetEmpty = "#ERR " & Err.Number
End Function

Public Function Np_RetNullStr() As Variant
    On Error GoTo EH
    Np_RetNullStr = (StrPtr(RetNullStr()) <> 0)
    Exit Function
EH:
    Np_RetNullStr = "#ERR " & Err.Number
End Function

Public Function Np_RetParamNever() As Variant
    On Error GoTo EH
    Dim s As String
    Np_RetParamNever = (StrPtr(RetParam(s)) <> 0)
    Exit Function
EH:
    Np_RetParamNever = "#ERR " & Err.Number
End Function

Public Function Np_RetParamEmpty() As Variant
    On Error GoTo EH
    Np_RetParamEmpty = (StrPtr(RetParam("")) <> 0)
    Exit Function
EH:
    Np_RetParamEmpty = "#ERR " & Err.Number
End Function

Public Function Np_RetVariant() As Variant
    On Error GoTo EH
    Np_RetVariant = (StrPtr(RetVariant()) <> 0)
    Exit Function
EH:
    Np_RetVariant = "#ERR " & Err.Number
End Function

Public Function Np_LiteralEmpty() As Variant
    On Error GoTo EH
    Np_LiteralEmpty = (StrPtr("") <> 0)
    Exit Function
EH:
    Np_LiteralEmpty = "#ERR " & Err.Number
End Function

Public Function Np_VbNullStringConst() As Variant
    On Error GoTo EH
    Np_VbNullStringConst = (StrPtr(vbNullString) <> 0)
    Exit Function
EH:
    Np_VbNullStringConst = "#ERR " & Err.Number
End Function

Public Function Np_VariantNever() As Variant
    On Error GoTo EH
    Dim v As Variant
    Np_VariantNever = (StrPtr(CStr(v)) <> 0)
    Exit Function
EH:
    Np_VariantNever = "#ERR " & Err.Number
End Function

Public Function Np_VariantAssignedNullStr() As Variant
    On Error GoTo EH
    Dim v As Variant
    v = vbNullString
    Np_VariantAssignedNullStr = (StrPtr(v) <> 0)
    Exit Function
EH:
    Np_VariantAssignedNullStr = "#ERR " & Err.Number
End Function

Public Function Np_VariantAssignedEmpty() As Variant
    On Error GoTo EH
    Dim v As Variant
    v = ""
    Np_VariantAssignedEmpty = (StrPtr(v) <> 0)
    Exit Function
EH:
    Np_VariantAssignedEmpty = "#ERR " & Err.Number
End Function

Public Function Np_VariantAssignedNever() As Variant
    On Error GoTo EH
    Dim v As Variant, s As String
    v = s
    Np_VariantAssignedNever = (StrPtr(v) <> 0)
    Exit Function
EH:
    Np_VariantAssignedNever = "#ERR " & Err.Number
End Function

Public Function Np_VariantArrElem() As Variant
    On Error GoTo EH
    Dim v As Variant
    v = Array("")
    Np_VariantArrElem = (StrPtr(v(0)) <> 0)
    Exit Function
EH:
    Np_VariantArrElem = "#ERR " & Err.Number
End Function

Public Function Np_VariantArrNullStr() As Variant
    On Error GoTo EH
    Dim v As Variant
    v = Array(vbNullString)
    Np_VariantArrNullStr = (StrPtr(v(0)) <> 0)
    Exit Function
EH:
    Np_VariantArrNullStr = "#ERR " & Err.Number
End Function

Public Function Np_VariantArrNever() As Variant
    On Error GoTo EH
    Dim v As Variant, s As String
    v = Array(s)
    Np_VariantArrNever = (StrPtr(v(0)) <> 0)
    Exit Function
EH:
    Np_VariantArrNever = "#ERR " & Err.Number
End Function

Public Function Np_SplitEmptyPiece() As Variant
    On Error GoTo EH
    Dim a() As String
    a = Split("a,,b", ",")
    Np_SplitEmptyPiece = (StrPtr(a(1)) <> 0)
    Exit Function
EH:
    Np_SplitEmptyPiece = "#ERR " & Err.Number
End Function

Public Function Np_VariantArrayElemNever() As Variant
    On Error GoTo EH
    Dim v(1 To 2) As Variant, s As String
    v(1) = s
    Np_VariantArrayElemNever = (StrPtr(v(1)) <> 0)
    Exit Function
EH:
    Np_VariantArrayElemNever = "#ERR " & Err.Number
End Function

Public Function Np_CollectionItem() As Variant
    On Error GoTo EH
    Dim c As New Collection, s As String
    c.Add s
    Np_CollectionItem = (StrPtr(c(1)) <> 0)
    Exit Function
EH:
    Np_CollectionItem = "#ERR " & Err.Number
End Function

Public Function Np_CollectionItemEmpty() As Variant
    On Error GoTo EH
    Dim c As New Collection
    c.Add ""
    Np_CollectionItemEmpty = (StrPtr(c(1)) <> 0)
    Exit Function
EH:
    Np_CollectionItemEmpty = "#ERR " & Err.Number
End Function

Public Function Np_ByRefPassNever() As Variant
    On Error GoTo EH
    Dim s As String
    Np_ByRefPassNever = ByRefPass(s)
    Exit Function
EH:
    Np_ByRefPassNever = "#ERR " & Err.Number
End Function

Public Function Np_ByRefPassEmpty() As Variant
    On Error GoTo EH
    Dim s As String
    s = ""
    Np_ByRefPassEmpty = ByRefPass(s)
    Exit Function
EH:
    Np_ByRefPassEmpty = "#ERR " & Err.Number
End Function

Public Function Np_ByValPassNever() As Variant
    On Error GoTo EH
    Dim s As String
    Np_ByValPassNever = ByValPass(s)
    Exit Function
EH:
    Np_ByValPassNever = "#ERR " & Err.Number
End Function

Public Function Np_ByValPassEmpty() As Variant
    On Error GoTo EH
    Dim s As String
    s = ""
    Np_ByValPassEmpty = ByValPass(s)
    Exit Function
EH:
    Np_ByValPassEmpty = "#ERR " & Err.Number
End Function

Public Function Np_ByValPassLiteral() As Variant
    On Error GoTo EH
    Np_ByValPassLiteral = ByValPass("")
    Exit Function
EH:
    Np_ByValPassLiteral = "#ERR " & Err.Number
End Function

Public Function Np_ByValPassNullStr() As Variant
    On Error GoTo EH
    Np_ByValPassNullStr = ByValPass(vbNullString)
    Exit Function
EH:
    Np_ByValPassNullStr = "#ERR " & Err.Number
End Function

Public Function Np_VarPassNever() As Variant
    On Error GoTo EH
    Dim s As String
    Np_VarPassNever = VarPass(s)
    Exit Function
EH:
    Np_VarPassNever = "#ERR " & Err.Number
End Function

Public Function Np_VarPassEmpty() As Variant
    On Error GoTo EH
    Np_VarPassEmpty = VarPass("")
    Exit Function
EH:
    Np_VarPassEmpty = "#ERR " & Err.Number
End Function

Public Function Np_VarPassNullStr() As Variant
    On Error GoTo EH
    Np_VarPassNullStr = VarPass(vbNullString)
    Exit Function
EH:
    Np_VarPassNullStr = "#ERR " & Err.Number
End Function

Public Function Np_FixedLenString() As Variant
    On Error GoTo EH
    Dim s As String * 3
    Np_FixedLenString = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_FixedLenString = "#ERR " & Err.Number
End Function

Public Function Np_ForEachElem() As Variant
    On Error GoTo EH
    Dim a(1 To 1) As String, e As Variant
    For Each e In a: Exit For: Next e
    Np_ForEachElem = (StrPtr(e) <> 0)
    Exit Function
EH:
    Np_ForEachElem = "#ERR " & Err.Number
End Function

Public Function Np_Lset() As Variant
    On Error GoTo EH
    Dim s As String
    s = "abc": LSet s = ""
    Np_Lset = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_Lset = "#ERR " & Err.Number
End Function

Public Function Np_ErrDescriptionClear() As Variant
    On Error GoTo EH
    Err.Clear
    Np_ErrDescriptionClear = (StrPtr(Err.Description) <> 0)
    Exit Function
EH:
    Np_ErrDescriptionClear = "#ERR " & Err.Number
End Function

Public Function Np_ErrSourceClear() As Variant
    On Error GoTo EH
    Err.Clear
    Np_ErrSourceClear = (StrPtr(Err.Source) <> 0)
    Exit Function
EH:
    Np_ErrSourceClear = "#ERR " & Err.Number
End Function

Public Function Np_CatNeverNever() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    Np_CatNeverNever = (StrPtr(s & t) <> 0)
    Exit Function
EH:
    Np_CatNeverNever = "#ERR " & Err.Number
End Function

Public Function Np_CatNeverEmpty() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    Np_CatNeverEmpty = (StrPtr(s & "") <> 0)
    Exit Function
EH:
    Np_CatNeverEmpty = "#ERR " & Err.Number
End Function

Public Function Np_CatEmptyNever() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    Np_CatEmptyNever = (StrPtr("" & s) <> 0)
    Exit Function
EH:
    Np_CatEmptyNever = "#ERR " & Err.Number
End Function

Public Function Np_CatEmptyEmpty() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    Np_CatEmptyEmpty = (StrPtr("" & "") <> 0)
    Exit Function
EH:
    Np_CatEmptyEmpty = "#ERR " & Err.Number
End Function

Public Function Np_CatNullStrNullStr() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    Np_CatNullStrNullStr = (StrPtr(vbNullString & vbNullString) <> 0)
    Exit Function
EH:
    Np_CatNullStrNullStr = "#ERR " & Err.Number
End Function

Public Function Np_CatNeverNullStr() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    Np_CatNeverNullStr = (StrPtr(s & vbNullString) <> 0)
    Exit Function
EH:
    Np_CatNeverNullStr = "#ERR " & Err.Number
End Function

Public Function Np_CatPlusNever() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    Np_CatPlusNever = (StrPtr(s + t) <> 0)
    Exit Function
EH:
    Np_CatPlusNever = "#ERR " & Err.Number
End Function

Public Function Np_CatPlusEmpty() As Variant
    On Error GoTo EH
    Dim s As String, t As String
    Np_CatPlusEmpty = (StrPtr("" + "") <> 0)
    Exit Function
EH:
    Np_CatPlusEmpty = "#ERR " & Err.Number
End Function

Public Function Np_CatAssign() As Variant
    On Error GoTo EH
    Dim s As String
    s = s & ""
    Np_CatAssign = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_CatAssign = "#ERR " & Err.Number
End Function

Public Function Np_CatAssignEmpty() As Variant
    On Error GoTo EH
    Dim s As String
    s = "": s = s & ""
    Np_CatAssignEmpty = (StrPtr(s) <> 0)
    Exit Function
EH:
    Np_CatAssignEmpty = "#ERR " & Err.Number
End Function

Public Function Np_CatVariantEmptyEmpty() As Variant
    On Error GoTo EH
    Dim v As Variant
    Np_CatVariantEmptyEmpty = (StrPtr(v & v) <> 0)
    Exit Function
EH:
    Np_CatVariantEmptyEmpty = "#ERR " & Err.Number
End Function

Public Function Np_CatVariantStr() As Variant
    On Error GoTo EH
    Dim v As Variant
    v = ""
    Np_CatVariantStr = (StrPtr(v & "") <> 0)
    Exit Function
EH:
    Np_CatVariantStr = "#ERR " & Err.Number
End Function

Public Function Np_Left0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Left0 = (StrPtr(Left("abc", 0)) <> 0)
    Exit Function
EH:
    Np_Left0 = "#ERR " & Err.Number
End Function

Public Function Np_Left1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Left1 = (StrPtr(Left("", 1)) <> 0)
    Exit Function
EH:
    Np_Left1 = "#ERR " & Err.Number
End Function

Public Function Np_Left2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Left2 = (StrPtr(Left(vbNullString, 1)) <> 0)
    Exit Function
EH:
    Np_Left2 = "#ERR " & Err.Number
End Function

Public Function Np_Left3() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Left3 = (StrPtr(Left("abc", 5)) <> 0)
    Exit Function
EH:
    Np_Left3 = "#ERR " & Err.Number
End Function

Public Function Np_Right0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Right0 = (StrPtr(Right("abc", 0)) <> 0)
    Exit Function
EH:
    Np_Right0 = "#ERR " & Err.Number
End Function

Public Function Np_Right1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Right1 = (StrPtr(Right("", 1)) <> 0)
    Exit Function
EH:
    Np_Right1 = "#ERR " & Err.Number
End Function

Public Function Np_Right2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Right2 = (StrPtr(Right(vbNullString, 1)) <> 0)
    Exit Function
EH:
    Np_Right2 = "#ERR " & Err.Number
End Function

Public Function Np_Mid0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Mid0 = (StrPtr(Mid("abc", 4)) <> 0)
    Exit Function
EH:
    Np_Mid0 = "#ERR " & Err.Number
End Function

Public Function Np_Mid1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Mid1 = (StrPtr(Mid("abc", 2, 0)) <> 0)
    Exit Function
EH:
    Np_Mid1 = "#ERR " & Err.Number
End Function

Public Function Np_Mid2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Mid2 = (StrPtr(Mid("", 1)) <> 0)
    Exit Function
EH:
    Np_Mid2 = "#ERR " & Err.Number
End Function

Public Function Np_Mid3() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Mid3 = (StrPtr(Mid(vbNullString, 1)) <> 0)
    Exit Function
EH:
    Np_Mid3 = "#ERR " & Err.Number
End Function

Public Function Np_Mid4() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Mid4 = (StrPtr(Mid("abc", 3, 0)) <> 0)
    Exit Function
EH:
    Np_Mid4 = "#ERR " & Err.Number
End Function

Public Function Np_LeftS0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_LeftS0 = (StrPtr(Left$("abc", 0)) <> 0)
    Exit Function
EH:
    Np_LeftS0 = "#ERR " & Err.Number
End Function

Public Function Np_LeftS1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_LeftS1 = (StrPtr(Left$(vbNullString, 1)) <> 0)
    Exit Function
EH:
    Np_LeftS1 = "#ERR " & Err.Number
End Function

Public Function Np_RightS0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_RightS0 = (StrPtr(Right$("abc", 0)) <> 0)
    Exit Function
EH:
    Np_RightS0 = "#ERR " & Err.Number
End Function

Public Function Np_MidS0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_MidS0 = (StrPtr(Mid$("abc", 4)) <> 0)
    Exit Function
EH:
    Np_MidS0 = "#ERR " & Err.Number
End Function

Public Function Np_MidS1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_MidS1 = (StrPtr(Mid$("abc", 2, 0)) <> 0)
    Exit Function
EH:
    Np_MidS1 = "#ERR " & Err.Number
End Function

Public Function Np_Trim0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Trim0 = (StrPtr(Trim("  ")) <> 0)
    Exit Function
EH:
    Np_Trim0 = "#ERR " & Err.Number
End Function

Public Function Np_Trim1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Trim1 = (StrPtr(Trim("")) <> 0)
    Exit Function
EH:
    Np_Trim1 = "#ERR " & Err.Number
End Function

Public Function Np_Trim2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Trim2 = (StrPtr(Trim(vbNullString)) <> 0)
    Exit Function
EH:
    Np_Trim2 = "#ERR " & Err.Number
End Function

Public Function Np_LTrim0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_LTrim0 = (StrPtr(LTrim("  ")) <> 0)
    Exit Function
EH:
    Np_LTrim0 = "#ERR " & Err.Number
End Function

Public Function Np_LTrim1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_LTrim1 = (StrPtr(LTrim("")) <> 0)
    Exit Function
EH:
    Np_LTrim1 = "#ERR " & Err.Number
End Function

Public Function Np_LTrim2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_LTrim2 = (StrPtr(LTrim(vbNullString)) <> 0)
    Exit Function
EH:
    Np_LTrim2 = "#ERR " & Err.Number
End Function

Public Function Np_RTrim0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_RTrim0 = (StrPtr(RTrim("  ")) <> 0)
    Exit Function
EH:
    Np_RTrim0 = "#ERR " & Err.Number
End Function

Public Function Np_RTrim1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_RTrim1 = (StrPtr(RTrim("")) <> 0)
    Exit Function
EH:
    Np_RTrim1 = "#ERR " & Err.Number
End Function

Public Function Np_RTrim2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_RTrim2 = (StrPtr(RTrim(vbNullString)) <> 0)
    Exit Function
EH:
    Np_RTrim2 = "#ERR " & Err.Number
End Function

Public Function Np_TrimS0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_TrimS0 = (StrPtr(Trim$("  ")) <> 0)
    Exit Function
EH:
    Np_TrimS0 = "#ERR " & Err.Number
End Function

Public Function Np_TrimS1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_TrimS1 = (StrPtr(Trim$("")) <> 0)
    Exit Function
EH:
    Np_TrimS1 = "#ERR " & Err.Number
End Function

Public Function Np_UCase0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_UCase0 = (StrPtr(UCase("")) <> 0)
    Exit Function
EH:
    Np_UCase0 = "#ERR " & Err.Number
End Function

Public Function Np_UCase1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_UCase1 = (StrPtr(UCase(vbNullString)) <> 0)
    Exit Function
EH:
    Np_UCase1 = "#ERR " & Err.Number
End Function

Public Function Np_LCase0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_LCase0 = (StrPtr(LCase("")) <> 0)
    Exit Function
EH:
    Np_LCase0 = "#ERR " & Err.Number
End Function

Public Function Np_LCase1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_LCase1 = (StrPtr(LCase(vbNullString)) <> 0)
    Exit Function
EH:
    Np_LCase1 = "#ERR " & Err.Number
End Function

Public Function Np_UCaseS0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_UCaseS0 = (StrPtr(UCase$("")) <> 0)
    Exit Function
EH:
    Np_UCaseS0 = "#ERR " & Err.Number
End Function

Public Function Np_UCaseS1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_UCaseS1 = (StrPtr(UCase$(vbNullString)) <> 0)
    Exit Function
EH:
    Np_UCaseS1 = "#ERR " & Err.Number
End Function

Public Function Np_LCaseS0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_LCaseS0 = (StrPtr(LCase$("")) <> 0)
    Exit Function
EH:
    Np_LCaseS0 = "#ERR " & Err.Number
End Function

Public Function Np_Space0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Space0 = (StrPtr(Space(0)) <> 0)
    Exit Function
EH:
    Np_Space0 = "#ERR " & Err.Number
End Function

Public Function Np_SpaceS0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_SpaceS0 = (StrPtr(Space$(0)) <> 0)
    Exit Function
EH:
    Np_SpaceS0 = "#ERR " & Err.Number
End Function

Public Function Np_String0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_String0 = (StrPtr(String(0, "x")) <> 0)
    Exit Function
EH:
    Np_String0 = "#ERR " & Err.Number
End Function

Public Function Np_StringS0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_StringS0 = (StrPtr(String$(0, "x")) <> 0)
    Exit Function
EH:
    Np_StringS0 = "#ERR " & Err.Number
End Function

Public Function Np_StrReverse0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_StrReverse0 = (StrPtr(StrReverse("")) <> 0)
    Exit Function
EH:
    Np_StrReverse0 = "#ERR " & Err.Number
End Function

Public Function Np_StrReverse1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_StrReverse1 = (StrPtr(StrReverse(vbNullString)) <> 0)
    Exit Function
EH:
    Np_StrReverse1 = "#ERR " & Err.Number
End Function

Public Function Np_CStr0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_CStr0 = (StrPtr(CStr("")) <> 0)
    Exit Function
EH:
    Np_CStr0 = "#ERR " & Err.Number
End Function

Public Function Np_CStr1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_CStr1 = (StrPtr(CStr(vbNullString)) <> 0)
    Exit Function
EH:
    Np_CStr1 = "#ERR " & Err.Number
End Function

Public Function Np_CStr2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_CStr2 = (StrPtr(CStr(Empty)) <> 0)
    Exit Function
EH:
    Np_CStr2 = "#ERR " & Err.Number
End Function

Public Function Np_CStr3() As Variant
    On Error GoTo EH
    Dim s As String
    Np_CStr3 = (StrPtr(CStr(s)) <> 0)
    Exit Function
EH:
    Np_CStr3 = "#ERR " & Err.Number
End Function

Public Function Np_Str0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Str0 = (StrPtr(Str(Empty)) <> 0)
    Exit Function
EH:
    Np_Str0 = "#ERR " & Err.Number
End Function

Public Function Np_Format0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Format0 = (StrPtr(Format("")) <> 0)
    Exit Function
EH:
    Np_Format0 = "#ERR " & Err.Number
End Function

Public Function Np_Format1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Format1 = (StrPtr(Format(vbNullString)) <> 0)
    Exit Function
EH:
    Np_Format1 = "#ERR " & Err.Number
End Function

Public Function Np_Format2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Format2 = (StrPtr(Format(Empty)) <> 0)
    Exit Function
EH:
    Np_Format2 = "#ERR " & Err.Number
End Function

Public Function Np_Format3() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Format3 = (StrPtr(Format(Null)) <> 0)
    Exit Function
EH:
    Np_Format3 = "#ERR " & Err.Number
End Function

Public Function Np_Format4() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Format4 = (StrPtr(Format("", "@")) <> 0)
    Exit Function
EH:
    Np_Format4 = "#ERR " & Err.Number
End Function

Public Function Np_Format5() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Format5 = (StrPtr(Format(5, ";;;")) <> 0)
    Exit Function
EH:
    Np_Format5 = "#ERR " & Err.Number
End Function

Public Function Np_Format6() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Format6 = (StrPtr(Format("", "@@;")) <> 0)
    Exit Function
EH:
    Np_Format6 = "#ERR " & Err.Number
End Function

Public Function Np_Format7() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Format7 = (StrPtr(Format(Null, ";;;")) <> 0)
    Exit Function
EH:
    Np_Format7 = "#ERR " & Err.Number
End Function

Public Function Np_FormatS0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_FormatS0 = (StrPtr(Format$("")) <> 0)
    Exit Function
EH:
    Np_FormatS0 = "#ERR " & Err.Number
End Function

Public Function Np_FormatS1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_FormatS1 = (StrPtr(Format$(Empty)) <> 0)
    Exit Function
EH:
    Np_FormatS1 = "#ERR " & Err.Number
End Function

Public Function Np_FormatS2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_FormatS2 = (StrPtr(Format$(Null)) <> 0)
    Exit Function
EH:
    Np_FormatS2 = "#ERR " & Err.Number
End Function

Public Function Np_Replace0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Replace0 = (StrPtr(Replace("abc", "abc", "")) <> 0)
    Exit Function
EH:
    Np_Replace0 = "#ERR " & Err.Number
End Function

Public Function Np_Replace1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Replace1 = (StrPtr(Replace("", "a", "b")) <> 0)
    Exit Function
EH:
    Np_Replace1 = "#ERR " & Err.Number
End Function

Public Function Np_Replace2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Replace2 = (StrPtr(Replace(vbNullString, "a", "b")) <> 0)
    Exit Function
EH:
    Np_Replace2 = "#ERR " & Err.Number
End Function

Public Function Np_Replace3() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Replace3 = (StrPtr(Replace("abc", "", "x", 5)) <> 0)
    Exit Function
EH:
    Np_Replace3 = "#ERR " & Err.Number
End Function

Public Function Np_Replace4() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Replace4 = (StrPtr(Replace("abc", "b", "x", 5)) <> 0)
    Exit Function
EH:
    Np_Replace4 = "#ERR " & Err.Number
End Function

Public Function Np_Replace5() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Replace5 = (StrPtr(Replace("abc", "", "x", 4)) <> 0)
    Exit Function
EH:
    Np_Replace5 = "#ERR " & Err.Number
End Function

Public Function Np_Replace6() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Replace6 = (StrPtr(Replace("abc", "abc", "", 1, 0)) <> 0)
    Exit Function
EH:
    Np_Replace6 = "#ERR " & Err.Number
End Function

Public Function Np_Replace7() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Replace7 = (StrPtr(Replace("abc", "x", "y", 4)) <> 0)
    Exit Function
EH:
    Np_Replace7 = "#ERR " & Err.Number
End Function

Public Function Np_Join0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Join0 = (StrPtr(Join(Array(), ",")) <> 0)
    Exit Function
EH:
    Np_Join0 = "#ERR " & Err.Number
End Function

Public Function Np_Join1() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Join1 = (StrPtr(Join(Split("", ","), "-")) <> 0)
    Exit Function
EH:
    Np_Join1 = "#ERR " & Err.Number
End Function

Public Function Np_Join2() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Join2 = (StrPtr(Join(Array(""), ",")) <> 0)
    Exit Function
EH:
    Np_Join2 = "#ERR " & Err.Number
End Function

Public Function Np_Join3() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Join3 = (StrPtr(Join(Array("", ""), "")) <> 0)
    Exit Function
EH:
    Np_Join3 = "#ERR " & Err.Number
End Function

Public Function Np_Join4() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Join4 = (StrPtr(Join(Array(vbNullString), ",")) <> 0)
    Exit Function
EH:
    Np_Join4 = "#ERR " & Err.Number
End Function

Public Function Np_Join5() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Join5 = (StrPtr(Join(Split("a,b", ","), "")) <> 0)
    Exit Function
EH:
    Np_Join5 = "#ERR " & Err.Number
End Function

Public Function Np_Hex0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Hex0 = (StrPtr(Hex(0)) <> 0)
    Exit Function
EH:
    Np_Hex0 = "#ERR " & Err.Number
End Function

Public Function Np_Oct0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Oct0 = (StrPtr(Oct(0)) <> 0)
    Exit Function
EH:
    Np_Oct0 = "#ERR " & Err.Number
End Function

Public Function Np_Chr0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_Chr0 = (StrPtr(Chr(65)) <> 0)
    Exit Function
EH:
    Np_Chr0 = "#ERR " & Err.Number
End Function

Public Function Np_ChrW0() As Variant
    On Error GoTo EH
    Dim s As String
    Np_ChrW0 = (StrPtr(ChrW(65)) <> 0)
    Exit Function
EH:
    Np_ChrW0 = "#ERR " & Err.Number
End Function

Public Function Np_SplitElemEmpty() As Variant
    On Error GoTo EH
    Np_SplitElemEmpty = (StrPtr(Split("a,,b", ",")(1)) <> 0)
    Exit Function
EH:
    Np_SplitElemEmpty = "#ERR " & Err.Number
End Function

Public Function Np_SplitElemSingle() As Variant
    On Error GoTo EH
    Np_SplitElemSingle = (StrPtr(Split("abc", ",")(0)) <> 0)
    Exit Function
EH:
    Np_SplitElemSingle = "#ERR " & Err.Number
End Function

Public Function Np_JoinSplitEmptyPieces() As Variant
    On Error GoTo EH
    Np_JoinSplitEmptyPieces = (StrPtr(Join(Split("a,,b", ","), "")) <> 0)
    Exit Function
EH:
    Np_JoinSplitEmptyPieces = "#ERR " & Err.Number
End Function

Public Function Np_SpaceThenTrim() As Variant
    On Error GoTo EH
    Np_SpaceThenTrim = (StrPtr(Trim(Space(3))) <> 0)
    Exit Function
EH:
    Np_SpaceThenTrim = "#ERR " & Err.Number
End Function

Public Function Np_LeftOfNeverVar() As Variant
    On Error GoTo EH
    Dim v As Variant, s As String
    v = s
    Np_LeftOfNeverVar = (StrPtr(Left(v, 1)) <> 0)
    Exit Function
EH:
    Np_LeftOfNeverVar = "#ERR " & Err.Number
End Function

Public Function Np_LeftOfEmptyVar() As Variant
    On Error GoTo EH
    Dim v As Variant
    v = ""
    Np_LeftOfEmptyVar = (StrPtr(Left(v, 1)) <> 0)
    Exit Function
EH:
    Np_LeftOfEmptyVar = "#ERR " & Err.Number
End Function

Public Function Np_UCaseOfNeverVar() As Variant
    On Error GoTo EH
    Dim v As Variant, s As String
    v = s
    Np_UCaseOfNeverVar = (StrPtr(UCase(v)) <> 0)
    Exit Function
EH:
    Np_UCaseOfNeverVar = "#ERR " & Err.Number
End Function

Public Function Np_MidOfNeverVar() As Variant
    On Error GoTo EH
    Dim v As Variant, s As String
    v = s
    Np_MidOfNeverVar = (StrPtr(Mid(v, 1)) <> 0)
    Exit Function
EH:
    Np_MidOfNeverVar = "#ERR " & Err.Number
End Function

Public Function Np_TrimOfNeverVar() As Variant
    On Error GoTo EH
    Dim v As Variant, s As String
    v = s
    Np_TrimOfNeverVar = (StrPtr(Trim(v)) <> 0)
    Exit Function
EH:
    Np_TrimOfNeverVar = "#ERR " & Err.Number
End Function

Public Function Np_CStrOfNeverVar() As Variant
    On Error GoTo EH
    Dim v As Variant, s As String
    v = s
    Np_CStrOfNeverVar = (StrPtr(CStr(v)) <> 0)
    Exit Function
EH:
    Np_CStrOfNeverVar = "#ERR " & Err.Number
End Function

Public Function Np_ReplaceOfNeverVar() As Variant
    On Error GoTo EH
    Dim v As Variant, s As String
    v = s
    Np_ReplaceOfNeverVar = (StrPtr(Replace(v, "a", "b")) <> 0)
    Exit Function
EH:
    Np_ReplaceOfNeverVar = "#ERR " & Err.Number
End Function

Public Function Np_IIfEmpty() As Variant
    On Error GoTo EH
    Np_IIfEmpty = (StrPtr(IIf(True, "", "x")) <> 0)
    Exit Function
EH:
    Np_IIfEmpty = "#ERR " & Err.Number
End Function

Public Function Np_IIfNullStr() As Variant
    On Error GoTo EH
    Np_IIfNullStr = (StrPtr(IIf(True, vbNullString, "x")) <> 0)
    Exit Function
EH:
    Np_IIfNullStr = "#ERR " & Err.Number
End Function

Public Function Np_IIfNever() As Variant
    On Error GoTo EH
    Dim s As String
    Np_IIfNever = (StrPtr(IIf(True, s, "x")) <> 0)
    Exit Function
EH:
    Np_IIfNever = "#ERR " & Err.Number
End Function
