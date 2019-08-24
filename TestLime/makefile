
LC=dotnet run -p ../limec/limec.fsproj --
LFLAGS=--target=ast

OBJS=Bool.ast Integer.ast List.ast Map.ast Test.ast

all: $(OBJS)

clean:
    del $(OBJS)

Bool.ast: Bool.lime
    $(LC) $(LFLAGS) "%s -> $@"

Integer.ast: Integer.lime
    $(LC) $(LFLAGS) "%s -> $@"

List.ast: List.lime
    $(LC) $(LFLAGS) "%s -> $@"

Map.ast: Map.lime List.ast
    $(LC) $(LFLAGS) "%s -> $@"

Test.ast: Test.lime Bool.ast Integer.ast List.ast Map.ast
    $(LC) $(LFLAGS) "%s -> $@"
