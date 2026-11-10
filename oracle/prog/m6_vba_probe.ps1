# Milestone 6 step 3 probe: VBA for macros. Class instances (Initialize/Terminate timing), As New,
# default members, Property procedures, Me, Event/RaiseEvent/WithEvents (sink order, ByRef Cancel),
# Implements and TypeOf, End, GoSub/Return, On ... GoTo, LSet/RSet, fixed-length strings, and the
# Application.Run name forms. Hidden Excel, Interactive=false, DisplayAlerts=false, events off; every
# Sub, handler, Class_Initialize and Class_Terminate traps errors and only logs to a string. The only
# cell writes are to Z1 of a plain sheet (the End test keeps its log there, since End clears LogS).
param([string]$Out)
$ErrorActionPreference = 'Stop'
Add-Type -Namespace P -Name W -MemberDefinition '[DllImport("user32.dll")] public static extern uint GetWindowThreadProcessId(IntPtr h, out uint pid);'
$before = @(Get-Process EXCEL -ErrorAction SilentlyContinue | ForEach-Object Id)
$xl = New-Object -ComObject Excel.Application
[uint32]$xpid = 0; [void][P.W]::GetWindowThreadProcessId([IntPtr][int]$xl.Hwnd, [ref]$xpid)
if ($before -contains [int]$xpid) { throw "attached to existing Excel $xpid" }
Set-Content -Path "$Out.pid" -Value $xpid
$xl.Visible = $false; $xl.DisplayAlerts = $false; $xl.Interactive = $false
$xl.AutomationSecurity = 1; $xl.ScreenUpdating = $false; $xl.EnableEvents = $false
$res = [ordered]@{}
$tmp = Join-Path ([IO.Path]::GetTempPath()) ("m6vba_" + [guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $tmp | Out-Null
function Add-Std($vbc, $name, $code) { $m = $vbc.Add(1); $m.Name = $name; $m.CodeModule.AddFromString($code) }
function Add-Cls($vbc, $name, $code) { $m = $vbc.Add(2); $m.Name = $name; $m.CodeModule.AddFromString($code) }
try {
  $wb = $xl.Workbooks.Add()
  $ws = $wb.Worksheets.Item(1)
  $vbc = $wb.VBProject.VBComponents
  Add-Std $vbc 'Log' @"
Public LogS As String
Public NextN As Long
Public G As CA
Public Keep As Object
Public Sub L(s As String)
    LogS = LogS & s & "|"
End Sub
Public Function GetLog() As String
    GetLog = LogS
    LogS = ""
End Function
Public Function NewId() As Long
    NextN = NextN + 1
    NewId = NextN
End Function
Public Function MakeOne() As CA
    Set MakeOne = New CA
End Function
Public Function Echo(ParamArray a() As Variant) As String
    Dim i As Long, s As String
    For i = LBound(a) To UBound(a)
        s = s & TypeName(a(i)) & ":" & a(i) & ","
    Next
    Echo = UBound(a) - LBound(a) + 1 & "|" & s
End Function
Public Function Typed(x As Long, Optional y As String = "d") As String
    Typed = TypeName(x) & ":" & x & " " & y
End Function
Public Sub Dup()
    L "Log.Dup"
End Sub
Public Sub EndHere()
    L "EndHere before"
    End
End Sub
Public Function EndFn() As Long
    EndFn = 5
    End
End Function
"@
  Add-Std $vbc 'M2' @"
Public Sub Dup()
    L "M2.Dup"
End Sub
Public Sub OnlyM2()
    L "OnlyM2"
End Sub
Private Sub PrivM2()
    L "PrivM2"
End Sub
"@
  Add-Cls $vbc 'CA' @"
Public Id As Long
Public Other As Object
Public Event Ping(ByVal n As Long, Cancel As Boolean)
Private Sub Class_Initialize()
    On Error Resume Next
    Id = NewId()
    L "Init " & Id
End Sub
Private Sub Class_Terminate()
    On Error Resume Next
    L "Term " & Id
End Sub
Public Property Get Name() As String
    Name = "CA" & Id
End Property
Public Function Self() As CA
    Set Self = Me
End Function
Public Function Fire(ByVal n As Long) As Boolean
    Dim c As Boolean
    RaiseEvent Ping(n, c)
    L "Fired " & n & " cancel=" & c
    Fire = c
End Function
Public Function MeType() As String
    MeType = TypeName(Me)
End Function
"@
  Add-Cls $vbc 'CS' @"
Public WithEvents Src As CA
Public Tag As String
Private Sub Src_Ping(ByVal n As Long, Cancel As Boolean)
    On Error Resume Next
    L "Sink " & Tag & " " & n & " cancel=" & Cancel
    If Tag = "c" Then Cancel = True
End Sub
"@
  Add-Cls $vbc 'IShape' @"
Public Function Area() As Double
End Function
Public Property Get Kind() As String
End Property
"@
  Add-Cls $vbc 'Sq' @"
Implements IShape
Private Function IShape_Area() As Double
    IShape_Area = 4
End Function
Private Property Get IShape_Kind() As String
    IShape_Kind = "square"
End Property
Public Function Side() As Double
    Side = 2
End Function
"@
  Add-Cls $vbc 'Bag' @"
Private mItems(1 To 3) As Variant
Private mObj As Object
Public Property Get Item(ByVal i As Long) As Variant
    Item = mItems(i)
End Property
Public Property Let Item(ByVal i As Long, ByVal v As Variant)
    mItems(i) = v
    L "Let Item " & i
End Property
Public Property Get Obj() As Object
    Set Obj = mObj
End Property
Public Property Set Obj(ByVal o As Object)
    Set mObj = o
    L "Set Obj"
End Property
"@
  $cls = @"
VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "CV"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Private mV As Variant
Public Property Get Value() As Variant
Attribute Value.VB_UserMemId = 0
    Value = mV
End Property
Public Property Let Value(ByVal v As Variant)
    mV = v
    L "Let Value " & TypeName(v)
End Property
"@
  [IO.File]::WriteAllText((Join-Path $tmp 'CV.cls'), ($cls -replace "`r?`n", "`r`n"), [Text.Encoding]::ASCII)
  [void]$vbc.Import((Join-Path $tmp 'CV.cls'))
  $vbc.Item($ws.CodeName).CodeModule.AddFromString(@"
Private WithEvents DocSrc As CA
Public Sub SetDocSrc(o As Object)
    Set DocSrc = o
End Sub
Public Sub SheetPub()
    L "SheetPub " & Me.Name & " " & TypeName(Me)
End Sub
Private Sub SheetPriv()
    L "SheetPriv"
End Sub
Private Sub DocSrc_Ping(ByVal n As Long, Cancel As Boolean)
    On Error Resume Next
    L "DocSink " & n
End Sub
"@)
  $vbc.Item($wb.CodeName).CodeModule.AddFromString(@"
Public Sub BookPub()
    L "BookPub " & Me.Name
End Sub
"@)
  Add-Std $vbc 'T' @"
Private Sub Inner(order As String)
    Dim a As CA, b As CA, c As CA
    If order = "abc" Then
        Set a = New CA: Set b = New CA: Set c = New CA
    Else
        Set c = New CA: Set b = New CA: Set a = New CA
    End If
    L "a=" & a.Id & " b=" & b.Id & " c=" & c.Id
End Sub
Public Sub P_LocalsAbc()
    On Error GoTo E
    Inner "abc"
    L "after"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_LocalsCba()
    On Error GoTo E
    Inner "cba"
    L "after"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_Replace()
    On Error GoTo E
    Dim x As CA
    Set x = New CA
    L "set1"
    Set x = New CA
    L "set2"
    Set x = Nothing
    L "nothing"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_TempA()
    On Error GoTo E
    L "name " & MakeOne().Name
    L "stmt2"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_TempB()
    On Error GoTo E
    MakeOne
    L "stmt2"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_TempC()
    On Error GoTo E
    Call MakeOne.Fire(1)
    L "stmt2"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_AsNew()
    On Error GoTo E
    Dim x As New CA
    L "decl"
    If x Is Nothing Then L "is nothing" Else L "not nothing"
    Set x = Nothing
    L "after nothing"
    L "id " & x.Id
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_Cycle()
    On Error GoTo E
    Dim a As CA, b As CA
    Set a = New CA: Set b = New CA
    Set a.Other = b: Set b.Other = a
    L "linked"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_Chain()
    On Error GoTo E
    Dim a As CA
    Set a = New CA
    Set a.Other = New CA
    Set a.Other.Other = New CA
    L "chain"
    Set a = Nothing
    L "released"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_Global()
    On Error GoTo E
    Set G = New CA
    Set Keep = G
    Set G = Nothing
    L "G released"
    Set Keep = Nothing
    L "Keep released"
    Set G = New CA
    L "G kept"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_Identity()
    On Error GoTo E
    Dim a As CA, b As CA, o As Object, v As Variant
    Set a = New CA
    Set b = a
    Set o = a
    Set v = a
    L "a is b " & (a Is b) & " a is self " & (a Is a.Self) & " o is a " & (o Is a) & " v is a " & (v Is a)
    L "typename " & TypeName(a) & " " & TypeName(o) & " " & TypeName(v) & " " & a.MeType & " " & VarType(v)
    L "isobject " & IsObject(v) & " nothing " & (a Is Nothing) & " typeof " & (TypeOf a Is CA) & (TypeOf o Is CS)
    Set a = Nothing: Set b = Nothing: Set o = Nothing
    L "v holds"
    v = Empty
    L "v empty"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_Events()
    On Error GoTo E
    Dim src As CA, s1 As CS, s2 As CS, s3 As CS
    Set src = New CA
    L "nosink " & src.Fire(1)
    Set s1 = New CS: s1.Tag = "1": Set s1.Src = src
    Set s2 = New CS: s2.Tag = "c": Set s2.Src = src
    Set s3 = New CS: s3.Tag = "3": Set s3.Src = src
    L "ret " & src.Fire(2)
    Set s2.Src = Nothing
    L "ret " & src.Fire(3)
    Set s2.Src = src
    L "ret " & src.Fire(4)
    Sheet1.SetDocSrc src
    L "ret " & src.Fire(5)
    Set s3 = Nothing
    L "ret " & src.Fire(6)
    Sheet1.SetDocSrc Nothing
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_Implements()
    On Error GoTo E
    Dim s As IShape, q As Sq, o As Object
    Set q = New Sq
    Set s = q
    L "area " & s.Area & " kind " & s.Kind & " side " & q.Side
    L "typename " & TypeName(s) & " typeof " & (TypeOf s Is IShape) & (TypeOf s Is Sq) & (TypeOf q Is IShape) & (TypeOf s Is CA)
    Set o = s
    L "late side " & o.Side
    L "late area " & o.Area
    Exit Sub
E:  L "ERR " & Err.Number & " " & Err.Description
    Resume Next
End Sub
Public Sub P_Default()
    On Error GoTo E
    Dim c As New CV, x As Variant, o As Object
    c = 5
    x = c
    L "x " & TypeName(x) & ":" & x & " str " & CStr(c) & " sum " & (c + 1)
    c.Value = "t"
    L "value " & c.Value & " eq " & (c = "t")
    Set o = c
    o = 7
    L "o " & c
    Exit Sub
E:  L "ERR " & Err.Number & " " & Err.Description
    Resume Next
End Sub
Public Sub P_Props()
    On Error GoTo E
    Dim b As New Bag, a As CA
    b.Item(2) = "two"
    L "item " & b.Item(2) & " " & TypeName(b.Item(1))
    Set a = New CA
    Set b.Obj = a
    L "obj " & b.Obj.Name
    L "item 4"
    b.Item(4) = 1
    Exit Sub
E:  L "ERR " & Err.Number & " " & Err.Description
    Resume Next
End Sub
Public Sub P_Doc()
    On Error GoTo E
    Sheet1.SheetPub
    ThisWorkbook.BookPub
    L "sheet name " & Sheet1.Name
    Exit Sub
E:  L "ERR " & Err.Number & " " & Err.Description
    Resume Next
End Sub
Public Sub P_GoSub()
    On Error GoTo E
    Dim i As Long
    For i = 0 To 4
        On i GoSub S1, S2, S3
        L "back " & i
    Next
    On 2 GoTo G2, G3
G2: L "G2"
G3: L "G3"
    i = -1
    On i GoTo G2, G3
    L "neg passed"
    Return
    L "after return"
    Exit Sub
S1: L "S1": Return
S2: L "S2": Return
S3: L "S3": Return
E:  L "ERR " & Err.Number
    Resume Next
End Sub
Public Sub P_Strings()
    On Error GoTo E
    Dim f As String * 5, v As String, n As Variant
    L "init [" & f & "] len " & Len(f) & " code " & Asc(f)
    f = "abcdefg"
    L "long [" & f & "]"
    f = "ab"
    L "short [" & f & "]"
    v = "123456"
    LSet v = "ab"
    L "lset [" & v & "]"
    RSet v = "ab"
    L "rset [" & v & "]"
    LSet v = "abcdefghij"
    L "lset long [" & v & "]"
    RSet v = "abcdefghij"
    L "rset long [" & v & "]"
    n = f
    L "variant " & TypeName(n) & " [" & n & "]"
    Exit Sub
E:  L "ERR " & Err.Number
    Resume Next
End Sub
Public Sub P_EndSet()
    On Error GoTo E
    Set G = New CA
    NextN = 100
    Sheet1.Range("Z1").Value = "set"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Sub P_EndRun()
    On Error GoTo E
    Sheet1.Range("Z1").Value = Sheet1.Range("Z1").Value & "|chain"
    EndHere
    Sheet1.Range("Z1").Value = Sheet1.Range("Z1").Value & "|after EndHere"
    Exit Sub
E:  L "ERR " & Err.Number
End Sub
Public Function P_EndState() As String
    On Error GoTo E
    P_EndState = "G nothing " & (G Is Nothing) & " NextN " & NextN & " log [" & LogS & "] Z1 " & Sheet1.Range("Z1").Value
    Exit Function
E:  P_EndState = "ERR " & Err.Number
End Function
Public Function P_Err() As String
    On Error Resume Next
    Err.Raise 5
    P_Err = "inner " & Err.Number
End Function
"@
  $wbn = $wb.Name
  function Get-Log() { try { [string]$xl.Run("'" + $wbn + "'!GetLog") } catch { 'LOGERR' } }
  function Fmt($r) {
    if ($null -eq $r) { return '<null>' }
    return (@($r) | ForEach-Object { if ($null -eq $_) { '<null>' } else { $_.GetType().Name + ':' + $_ } }) -join ','
  }
  function Step($label, [scriptblock]$b) {
    $err = ''; $r = $null
    Add-Content -Path "$Out.progress" -Value $label
    try { $r = & $b } catch {
      $ex = $_.Exception; while ($ex.InnerException) { $ex = $ex.InnerException }
      $err = ' ERR ' + ('{0:X8}' -f $ex.HResult) + ' ' + $ex.Message
    }
    $res[$label] = "ret=" + (Fmt $r) + ' log=' + (Get-Log) + $err
    Add-Content -Path "$Out.progress" -Value ("  " + $res[$label])
  }
  Add-Content -Path "$Out.progress" -Value 'modules added'
  [void](Get-Log)
  foreach ($p in 'P_LocalsAbc','P_LocalsCba','P_Replace','P_AsNew','P_Cycle','P_Chain','P_Global','P_Identity','P_Implements','P_Default','P_Props','P_Doc','P_GoSub','P_Strings') {
    Step $p { $xl.Run($p) }
  }
  Step 'run P_Err' { $xl.Run('P_Err') }
  Step 'run Dup' { $xl.Run('Dup') }
  Step 'run Log.Dup' { $xl.Run('Log.Dup') }
  Step 'run M2.Dup' { $xl.Run('M2.Dup') }
  Step 'run OnlyM2' { $xl.Run('OnlyM2') }
  Step 'run PrivM2' { $xl.Run('PrivM2') }
  Step 'run M2.PrivM2' { $xl.Run('M2.PrivM2') }
  Step 'run quoted book module' { $xl.Run("'$wbn'!M2.OnlyM2") }
  Step 'run bare book module' { $xl.Run("$wbn!M2.OnlyM2") }
  Step 'run bare book' { $xl.Run("$wbn!OnlyM2") }
  Step 'run SheetPub' { $xl.Run('SheetPub') }
  Step 'run Sheet1.SheetPub' { $xl.Run('Sheet1.SheetPub') }
  Step 'run Sheet1.SheetPriv' { $xl.Run('Sheet1.SheetPriv') }
  Step 'run ThisWorkbook.BookPub' { $xl.Run('ThisWorkbook.BookPub') }
  Step 'run BookPub' { $xl.Run('BookPub') }
  Step 'run CA.Fire' { $xl.Run('CA.Fire', 1) }
  Step 'run CA.Name' { $xl.Run('CA.Name') }
  Step 'run NoModule.OnlyM2' { $xl.Run('NoModule.OnlyM2') }
  Step 'run M2.Missing' { $xl.Run('M2.Missing') }
  Step 'run lower case' { $xl.Run('onlym2') }
  Step 'run spaced' { $xl.Run(' OnlyM2') }
  Step 'run Echo 0' { $xl.Run('Echo') }
  Step 'run Echo 3' { $xl.Run('Echo', 1, 'b', 2.5) }
  Step 'run Echo 30' { $xl.Run('Echo', 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30) }
  Step 'run Typed "5"' { $xl.Run('Typed', '5') }
  Step 'run Typed 2.5' { $xl.Run('Typed', 2.5) }
  Step 'run MakeOne' { $r = $xl.Run('MakeOne'); $t = $r.Name; $r = $null; [GC]::Collect(); [GC]::WaitForPendingFinalizers(); $t }
  Step 'run EndFn' { $xl.Run('EndFn') }
  Step 'run P_EndState after EndFn' { $xl.Run('P_EndState') }
  Step 'run P_EndSet' { $xl.Run('P_EndSet') }
  Step 'run P_EndRun' { $xl.Run('P_EndRun') }
  Step 'run P_EndState after End' { $xl.Run('P_EndState') }
  foreach ($p in 'P_TempA','P_TempB','P_Events','P_TempC') { Step $p { $xl.Run($p) } }
} catch {
  $res['FATAL'] = $_.Exception.Message + ' @ ' + $_.InvocationInfo.ScriptLineNumber
} finally {
  try { foreach ($w in @($xl.Workbooks)) { $w.Close($false) } } catch {}
  try { $xl.Quit() } catch {}
  [void][Runtime.InteropServices.Marshal]::ReleaseComObject($xl)
  [GC]::Collect(); [GC]::WaitForPendingFinalizers()
  Start-Sleep -Seconds 2
  Get-Process -Id $xpid -ErrorAction SilentlyContinue | Stop-Process -Force
  Remove-Item -Recurse -Force $tmp -ErrorAction SilentlyContinue
}
$res.GetEnumerator() | ForEach-Object { "{0} => {1}" -f $_.Key, $_.Value } | Set-Content -Path $Out
