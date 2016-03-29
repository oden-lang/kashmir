package importer

type Type interface {
}

type Pointer struct {
	Kind  string `json:"kind"`
	Inner Type   `json:"inner"`
}

func NewPointer(inner Type) Pointer {
	return Pointer{"pointer", inner}
}

type Array struct {
	Kind   string `json:"kind"`
	Inner  Type   `json:"inner"`
	Lenght int64  `json:"length"`
}

func NewArray(inner Type, length int64) Array {
	return Array{"array", inner, length}
}

type Slice struct {
	Kind  string `json:"kind"`
	Inner Type   `json:"inner"`
}

func NewSlice(inner Type) Slice {
	return Slice{"slice", inner}
}

type StructField struct {
	Kind string `json:"kind"`
	Name string `json:"name"`
	Type Type   `json:"type"`
}

func NewStructField(name string, fieldType Type) StructField {
	return StructField{"struct_field", name, fieldType}
}

type Struct struct {
	Kind   string        `json:"kind"`
	Fields []StructField `json:"fields"`
}

func NewStruct(fields []StructField) Struct {
	return Struct{"struct", fields}
}

type Signature struct {
	Kind     string `json:"kind"`
	Variadic bool   `json:"variadic"`
	Recv     *Type  `json:"recv"`
	Params   []Type `json:"params"`
	Returns  []Type `json:"returns"`
}

func NewSignature(variadic bool, recv *Type, arguments, returns []Type) Signature {
	return Signature{"signature", variadic, recv, arguments, returns}
}

type Basic struct {
	Kind    string `json:"kind"`
	Name    string `json:"name"`
	Untyped bool   `json:"untyped"`
}

func NewBasic(name string, untyped bool) Basic {
	return Basic{"basic", name, untyped}
}

type Named struct {
	Kind       string   `json:"kind"`
	Pkg        []string `json:"pkg"`
	Name       string   `json:"name"`
	Underlying Type     `json:"underlying"`
	// Methods []Func `json:"methods"`
}

func NewNamed(pkg []string, name string, underlying Type) Named {
	return Named{"named", pkg, name, underlying}
}

type Interface struct {
	Kind string `json:"kind"`
}

func NewInterface() Interface {
	return Interface{"interface"}
}

type Unsupported struct {
	Kind string `json:"kind"`
	Name string `json:"name"`
}

func NewUnsupported(name string) Unsupported {
	return Unsupported{"unsupported", name}
}

type Object struct {
	Name       string `json:"name"`
	ObjectType string `json:"objectType"`
	Type       Type   `json:"type"`
}

type Scope struct {
	Objects []Object `json:"objects"`
}
