
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

Map.ast: Map.lime
    $(LC) $(LFLAGS) "%s -> $@"

Test.ast: Test.lime
    $(LC) $(LFLAGS) "%s -> $@"
