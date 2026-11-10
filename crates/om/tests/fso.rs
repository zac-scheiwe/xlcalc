use std::path::PathBuf;

use xlcalc::Workbook;
use xlcalc_om::{flags, Application, Call};
use xlcalc_variant::Variant;
use xlcalc_vba::{ModuleKind, SourceModule};

struct TempDir(PathBuf);

impl TempDir {
    fn new(tag: &str) -> TempDir {
        let p = std::env::temp_dir().join(format!("xlc_fso_{tag}_{}", std::process::id()));
        std::fs::remove_dir_all(&p).ok();
        std::fs::create_dir_all(&p).unwrap();
        TempDir(std::fs::canonicalize(&p).unwrap())
    }

    fn path(&self) -> String {
        let s = self.0.to_string_lossy().to_string();
        s.strip_prefix(r"\\?\").map_or(s.clone(), str::to_string)
    }

    fn file(&self, name: &str) -> PathBuf {
        self.0.join(name)
    }

    fn put(&self, name: &str, bytes: &[u8]) {
        let p = self.file(name);
        std::fs::create_dir_all(p.parent().unwrap()).unwrap();
        std::fs::write(p, bytes).unwrap();
    }

    fn read(&self, name: &str) -> Vec<u8> {
        std::fs::read(self.file(name)).unwrap()
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.0).ok();
    }
}

fn run(src: &str, function: &str, dir: &TempDir) -> String {
    let mut app = Application::new();
    let book = app.add_workbook(Workbook::new());
    let module = SourceModule {
        name: "Module1".into(),
        kind: ModuleKind::Standard,
        source: src.into(),
    };
    let errors = app.add_vba_module(book, module).unwrap();
    assert!(errors.is_empty(), "{errors:?}");
    let h = app.handle();
    let call = Call {
        flags: flags::METHOD,
        lcid: 0x0409,
    };
    let args = [Variant::Bstr(function.into()), Variant::Bstr(dir.path())];
    match app.invoke(h, "Run", call, &args, &[]) {
        Ok(Variant::Bstr(s)) => s,
        other => panic!("{function}: {other:?}"),
    }
}

const VBA: &str = r#"
Function Wrote(d As String) As String
    Dim f As Object, t As Object, s As String, x As Variant
    Set f = CreateObject("Scripting.FileSystemObject")
    Set t = f.CreateTextFile(d & "\a.txt")
    s = TypeName(t) & ";"
    t.Write "abc"
    s = s & t.Line & "," & t.Column & ";"
    t.WriteLine "de"
    s = s & t.Line & "," & t.Column & ";"
    t.WriteBlankLines 1
    t.Write 12
    t.Write 1.5
    t.Write True
    t.WriteLine
    s = s & t.Line & "," & t.Column & ";"
    On Error Resume Next
    t.ReadAll
    s = s & Err.Number & ":" & Err.Description & ";": Err.Clear
    x = t.AtEndOfStream
    s = s & Err.Number & ";": Err.Clear
    t.Close
    t.Write "x"
    s = s & Err.Number & ";": Err.Clear
    t.Close
    s = s & Err.Number
    Wrote = s
End Function

Function Reads(d As String) As String
    Dim f As Object, t As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    On Error Resume Next
    Set t = f.OpenTextFile(d & "\r.txt")
    s = t.ReadLine & "[" & t.Line & "," & t.Column & "]"
    s = s & t.Read(2) & "[" & t.Line & "," & t.Column & "," & t.AtEndOfLine & "]"
    s = s & t.Read(2)
    t.Skip 1
    s = s & t.Line & "," & t.Column & ";"
    t.SkipLine
    s = s & t.Line & "," & t.Column & ";"
    t.Write "x"
    s = s & Err.Number & ":" & Err.Description & ";": Err.Clear
    s = s & t.ReadAll & "|" & t.AtEndOfStream & ";"
    t.ReadLine
    s = s & Err.Number & ":" & Err.Description & ";": Err.Clear
    t.Skip 1
    s = s & Err.Number & ";": Err.Clear
    s = s & "[" & t.Read(0) & "]" & Err.Number
    t.Close
    Reads = s
End Function

Function OpenErrors(d As String) As String
    Dim f As Object, t As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    On Error Resume Next
    Set t = f.CreateTextFile(d & "\r.txt", False)
    s = Err.Number & ":" & Err.Description & ";": Err.Clear
    Set t = f.OpenTextFile(d & "\nope.txt")
    s = s & Err.Number & ":" & Err.Description & ";": Err.Clear
    Set t = f.OpenTextFile(d & "\nodir\nope.txt", 2, True)
    s = s & Err.Number & ":" & Err.Description & ";": Err.Clear
    Set t = f.OpenTextFile(d & "\r.txt", 3)
    s = s & Err.Number & ";": Err.Clear
    Set t = f.CreateTextFile(d & "\a*.txt")
    s = s & Err.Number & ":" & Err.Description & ";": Err.Clear
    Set t = f.CreateTextFile(d)
    s = s & Err.Number & ":" & Err.Description
    OpenErrors = s
End Function

Function Lines(d As String) As String
    Dim f As Object, t As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    Set t = f.OpenTextFile(d & "\mix.txt")
    Do While Not t.AtEndOfStream
        s = s & "[" & Replace(Replace(t.ReadLine, vbCr, "\r"), vbLf, "\n") & "]" & t.Line & "," & t.Column
    Loop
    t.Close
    s = s & "|"
    Set t = f.OpenTextFile(d & "\mix.txt")
    Do While Not t.AtEndOfStream
        s = s & t.AtEndOfLine & ","
        t.Skip 1
    Loop
    t.Close
    Lines = s
End Function

Function Append(d As String) As String
    Dim f As Object, t As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    Set t = f.OpenTextFile(d & "\ap.txt", 8)
    s = t.Line & "," & t.Column
    t.WriteLine "c"
    t.Close
    Set t = f.OpenTextFile(d & "\ap.txt", 2)
    s = s & ";" & t.Line & "," & t.Column
    t.Close
    Set t = f.OpenTextFile(d & "\new.txt", 8, True, -1)
    t.Write "ab"
    t.Close
    Set t = f.OpenTextFile(d & "\new.txt", 8, False, -1)
    t.Write "c"
    t.Close
    Set t = f.OpenTextFile(d & "\new.txt", 1, False, -1)
    s = s & ";" & t.ReadAll
    t.Close
    Set t = f.OpenTextFile(d & "\new.txt", 1, False, -2)
    s = s & ";" & t.ReadAll
    t.Close
    Set t = f.OpenTextFile(d & "\new.txt", 1, False, 0)
    s = s & ";" & Len(t.ReadAll)
    t.Close
    Append = s
End Function

Function Sharing(d As String) As String
    Dim f As Object, w As Object, r As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    Set w = f.OpenTextFile(d & "\sh.txt", 2, True)
    On Error Resume Next
    Set r = f.OpenTextFile(d & "\sh.txt", 2)
    s = s & Err.Number & ";": Err.Clear
    Set r = f.OpenTextFile(d & "\sh.txt", 8)
    s = s & Err.Number & ";": Err.Clear
    f.DeleteFile d & "\sh.txt"
    s = s & Err.Number & ";": Err.Clear
    Set r = f.OpenTextFile(d & "\sh.txt", 1)
    s = s & Err.Number & ";": Err.Clear
    r.Close
    w.Close
    f.DeleteFile d & "\sh.txt"
    s = s & Err.Number & ";" & f.FileExists(d & "\sh.txt")
    Sharing = s
End Function

Function FolderTree(d As String) As String
    Dim f As Object, fo As Object, x As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    On Error GoTo bad
    Set fo = f.GetFolder(d & "\tree")
    s = TypeName(fo) & "|" & fo.Name & "|" & (fo.Path = d & "\tree") & "|" & fo.Attributes
    s = s & "|" & fo.IsRootFolder & "|" & fo.Size & "|" & fo.Type & "|" & (fo.ParentFolder.Path = d)
    s = s & "|" & fo.Files.Count & ":"
    For Each x In fo.Files
        s = s & x.Name & ","
    Next
    s = s & "|" & fo.SubFolders.Count & ":"
    For Each x In fo.SubFolders
        s = s & x.Name & ","
    Next
    s = s & "|" & fo.Files.Item("b.txt").Name & "|" & (fo.Files("A.TXT").Path = d & "\tree\a.txt")
    s = s & "|" & fo.SubFolders("SUB1").Size & "|" & fo.SubFolders("sub1").ParentFolder.IsRootFolder
    s = s & "|" & TypeName(fo.Files) & "|" & TypeName(fo.SubFolders)
    On Error Resume Next
    Set x = fo.Files("zzz")
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    Set x = fo.SubFolders("zzz")
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    Set x = f.GetFolder(d & "\zzz")
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    Set x = f.GetFile(d & "\tree\zzz")
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    Set x = f.GetFile(d & "\tree")
    s = s & "|" & Err.Number: Err.Clear
    Set x = f.GetFolder(d & "\tree\a.txt")
    s = s & "|" & Err.Number
    FolderTree = s
    Exit Function
bad:
    FolderTree = s & " ERR " & Err.Number & Err.Description
End Function

Function FileObjs(d As String) As String
    Dim f As Object, g As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    Set g = f.GetFile(d & "\TREE\A.TXT")
    s = TypeName(g) & "|" & g.Name & "|" & (g.Path = d & "\tree\a.txt") & "|" & g.Size & "|" & g.Type
    s = s & "|" & g.ShortName & "|" & g.Attributes & "|" & g.ParentFolder.Name & "|" & g.Drive.DriveLetter
    s = s & "|" & f.GetFile(d & "\tree\c").Type & "|" & f.GetFile(d & "\tree\x.xyz").Type
    s = s & "|" & VarType(g.DateLastModified)
    g.Attributes = 7
    s = s & "|" & g.Attributes
    g.Attributes = 0
    s = s & "|" & g.Attributes
    g.Attributes = 32
    On Error Resume Next
    g.Name = "c"
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    g.Name = "a*b"
    s = s & "|" & Err.Number: Err.Clear
    g.Name = "z.txt"
    s = s & "|" & g.Name & "|" & f.FileExists(d & "\tree\z.txt") & f.FileExists(d & "\tree\a.txt")
    g.Copy d & "\tree\sub1"
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    g.Copy d & "\tree\sub1\"
    s = s & "|" & Err.Number & f.FileExists(d & "\tree\sub1\z.txt")
    g.Copy d & "\tree\sub1\", False
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    g.Move d & "\tree\sub1\"
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    g.Move d & "\tree\Sub2\"
    s = s & "|" & (g.Path = d & "\tree\Sub2\z.txt") & f.FileExists(d & "\tree\z.txt")
    g.Attributes = 1
    g.Delete
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    g.Delete True
    s = s & "|" & Err.Number & f.FileExists(d & "\tree\Sub2\z.txt")
    s = s & g.Name
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    g.Delete
    s = s & "|" & Err.Number
    FileObjs = s
End Function

Function Fso(d As String) As String
    Dim f As Object, fo As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    On Error Resume Next
    Set fo = f.CreateFolder(d & "\cf\")
    s = TypeName(fo) & "|" & fo.Name & "|" & (fo.Path = d & "\cf")
    f.CreateFolder d & "\cf"
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    f.CreateFolder d & "\nodir\x"
    s = s & "|" & Err.Number: Err.Clear
    f.CreateFolder ""
    s = s & "|" & (Err.Number <> 0): Err.Clear
    f.CopyFile d & "\tree\a.txt", d & "\cf\"
    s = s & "|" & Err.Number & f.FileExists(d & "\cf\a.txt"): Err.Clear
    f.CopyFile d & "\tree\a.txt", d & "\cf\", False
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    f.CopyFile d & "\tree\a.txt", d & "\cf"
    s = s & "|" & Err.Number: Err.Clear
    f.CopyFile d & "\tree\*.txt", d & "\cf\"
    s = s & "|" & Err.Number & f.FileExists(d & "\cf\B.TXT"): Err.Clear
    f.CopyFile d & "\tree\nope.txt", d & "\cf\"
    s = s & "|" & Err.Number: Err.Clear
    f.CopyFile d & "\nodir\a.txt", d & "\cf\"
    s = s & "|" & Err.Number: Err.Clear
    f.MoveFile d & "\cf\a.txt", d & "\cf\m.txt"
    s = s & "|" & Err.Number & f.FileExists(d & "\cf\a.txt") & f.FileExists(d & "\cf\m.txt"): Err.Clear
    f.MoveFile d & "\cf\m.txt", d & "\cf\B.TXT"
    s = s & "|" & Err.Number: Err.Clear
    f.DeleteFile d & "\cf\*.txt"
    s = s & "|" & Err.Number & f.FileExists(d & "\cf\B.TXT"): Err.Clear
    f.DeleteFile d & "\cf\*.txt"
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    f.DeleteFile d & "\cf\nope.txt"
    s = s & "|" & Err.Number: Err.Clear
    f.CopyFolder d & "\tree", d & "\tree2"
    s = s & "|" & Err.Number & f.FileExists(d & "\tree2\sub1\deep\d.txt"): Err.Clear
    f.CopyFolder d & "\tree", d & "\tree2", False
    s = s & "|" & Err.Number: Err.Clear
    f.CopyFolder d & "\tree", d & "\tree\sub1\"
    s = s & "|" & Err.Number: Err.Clear
    f.CopyFolder d & "\tree", d & "\cf\"
    s = s & "|" & f.FolderExists(d & "\cf\tree") & f.FileExists(d & "\cf\a.txt"): Err.Clear
    f.MoveFolder d & "\tree2", d & "\cf\"
    s = s & "|" & Err.Number & f.FolderExists(d & "\tree2") & f.FolderExists(d & "\cf\tree2"): Err.Clear
    f.MoveFolder d & "\cf\tree2", d & "\tree"
    s = s & "|" & Err.Number: Err.Clear
    f.DeleteFolder d & "\nope"
    s = s & "|" & Err.Number: Err.Clear
    f.DeleteFolder d & "\cf"
    s = s & "|" & Err.Number & f.FolderExists(d & "\cf"): Err.Clear
    s = s & "|" & f.GetFolder(d & "\tree").SubFolders.Add("n").Name
    f.GetFolder(d & "\tree\n").Delete
    s = s & f.FolderExists(d & "\tree\n")
    Fso = s
End Function
"#;

#[test]
fn write_stream() {
    let d = TempDir::new("write");
    assert_eq!(
        run(VBA, "Wrote", &d),
        "TextStream;1,4;2,1;4,1;54:Bad file mode;54;91;0"
    );
    assert_eq!(d.read("a.txt"), b"abcde\r\n\r\n121.5True\r\n");
}

#[test]
fn read_stream() {
    let d = TempDir::new("read");
    d.put("r.txt", b"line1\r\nline2\r\nline3");
    assert_eq!(
        run(VBA, "Reads", &d),
        "line1[2,1]li[2,3,False]ne2,6;3,1;54:Bad file mode;line3|True;62:Input past end of file;62;[]0"
    );
}

#[test]
fn open_errors() {
    let d = TempDir::new("errors");
    d.put("r.txt", b"x");
    assert_eq!(
        run(VBA, "OpenErrors", &d),
        "58:File already exists;53:File not found;76:Path not found;-2147024809;\
52:Bad file name or number;70:Permission denied"
    );
}

#[test]
fn line_terminators() {
    let d = TempDir::new("lines");
    d.put("mix.txt", b"a\r\n\r\nb\n\rc\r\r\nd");
    assert_eq!(
        run(VBA, "Lines", &d),
        "[a]2,1[]3,1[b]4,1[\\rc\\r]5,1[d]5,2|\
False,True,True,True,True,False,True,False,False,False,True,True,False,"
    );
}

#[test]
fn append_and_formats() {
    let d = TempDir::new("append");
    d.put("ap.txt", b"ab\r\ncde");
    assert_eq!(run(VBA, "Append", &d), "2,4;1,1;abc;abc;8");
    assert_eq!(d.read("ap.txt"), b"");
    assert_eq!(d.read("new.txt"), [0xFF, 0xFE, 0x61, 0, 0x62, 0, 0x63, 0]);
}

#[test]
fn sharing_violations() {
    let d = TempDir::new("sharing");
    assert_eq!(run(VBA, "Sharing", &d), "70;70;70;0;0;False");
}

#[test]
fn folders_and_collections() {
    let d = TempDir::new("tree");
    d.put("tree/a.txt", b"hello");
    d.put("tree/B.TXT", b"abc");
    d.put("tree/c", b"x");
    d.put("tree/x.xyz", b"");
    d.put("tree/sub1/s1.txt", b"12345");
    d.put("tree/sub1/deep/d.txt", b"1234567890");
    std::fs::create_dir_all(d.file("tree/Sub2")).unwrap();
    assert_eq!(
        run(VBA, "FolderTree", &d),
        "Folder|tree|True|16|False|24|File folder|True|4:a.txt,B.TXT,c,x.xyz,|2:sub1,Sub2,|B.TXT|True|15|False|Files|Folders|53:File not found|76:Path not found|76:Path not found|53:File not found|53|76"
    );
}

#[test]
fn file_objects() {
    let d = TempDir::new("file");
    d.put("tree/a.txt", b"hello");
    d.put("tree/c", b"x");
    d.put("tree/x.xyz", b"");
    std::fs::create_dir_all(d.file("tree/sub1")).unwrap();
    std::fs::create_dir_all(d.file("tree/Sub2")).unwrap();
    assert_eq!(
        run(VBA, "FileObjs", &d),
        "File|a.txt|True|5|Text Document|a.txt|32|tree|C|File|XYZ File|7|7|0|58:File already exists|-2147024809|z.txt|TrueFalse|70:Permission denied|0True|58:File already exists|58:File already exists|TrueFalse|70:Permission denied|0False|53:File not found|53"
    );
}

#[test]
fn fso_file_and_folder_methods() {
    let d = TempDir::new("fso");
    d.put("tree/a.txt", b"hello");
    d.put("tree/B.TXT", b"abc");
    d.put("tree/sub1/deep/d.txt", b"1234567890");
    assert_eq!(
        run(VBA, "Fso", &d),
        "Folder|cf|True|58:File already exists|76|True|0True|58:File already exists|70|0True|53|76|0FalseTrue|58|0False|53:File not found|53|0True|58|5|TrueFalse|0FalseTrue|58|76|0False|nFalse"
    );
}

const VBA2: &str = r#"
Function Drives(d As String) As String
    Dim f As Object, s As String, x As Object
    Set f = CreateObject("Scripting.FileSystemObject")
    s = f.GetDrive("c").DriveLetter & "|" & f.GetDrive("C:\").Path & "|" & f.GetDrive("C:").DriveType
    s = s & "|" & f.GetDrive("C:").RootFolder.Path & "|" & f.GetFolder("C:\").IsRootFolder
    s = s & "|" & TypeName(f.GetDrive("C:")) & "|" & (Len(f.GetSpecialFolder(2).Path) > 0)
    s = s & "|" & f.DriveExists("C:\Windows") & f.DriveExists("c") & f.DriveExists("Q:")
    On Error Resume Next
    Set x = f.GetDrive("Q:")
    s = s & "|" & Err.Number: Err.Clear
    Set x = f.GetDrive("C:\Windows")
    s = s & "|" & Err.Number: Err.Clear
    Set x = f.GetSpecialFolder(3)
    s = s & "|" & Err.Number: Err.Clear
    Drives = s
End Function

Function Unicode(d As String) As String
    Dim f As Object, t As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    Set t = f.CreateTextFile(d & "\u.txt", True, True)
    t.Write "h" & ChrW(&H20AC)
    t.Close
    Set t = f.CreateTextFile(d & "\n.txt")
    t.Write "a" & ChrW(&HE9) & ChrW(&H20AC)
    On Error Resume Next
    t.Write "b" & ChrW(&H4E2D)
    s = Err.Number & ":" & Err.Description: Err.Clear
    t.Close
    Set t = f.OpenTextFile(d & "\u.txt", 1, False, -2)
    s = s & "|" & AscW(Right(t.ReadAll, 1))
    t.Close
    Set t = f.OpenTextFile(d & "\n.txt", 1)
    s = s & "|" & AscW(Mid(t.ReadAll, 3, 1))
    Unicode = s
End Function
"#;

#[test]
fn drives_special_folders() {
    let d = TempDir::new("drives");
    assert_eq!(
        run(VBA2, "Drives", &d),
        r"C|C:|2|C:\|True|Drive|True|FalseTrueFalse|68|-2147024809|-2147024809"
    );
}

#[test]
fn unicode_and_ansi_files() {
    let d = TempDir::new("unicode");
    assert_eq!(
        run(VBA2, "Unicode", &d),
        "5:Invalid procedure call or argument|8364|8364"
    );
    assert_eq!(d.read("u.txt"), [0xFF, 0xFE, 0x68, 0, 0xAC, 0x20]);
    assert_eq!(d.read("n.txt"), [0x61, 0xE9, 0x80]);
}
