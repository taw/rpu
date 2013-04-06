#!/usr/bin/perl -w

my @out_vtx;     my %cached_vtx = ();
my @out_tridata;
my @out_tri;

my @v  = ();
my @vn = ();
my @vt = ();
my $cur_material = 0;

$mtl_path = "../scene/";
#$obj_file = "../scene/edgar_0.obj";
#$obj_file = "../scene/hand_0.obj";
#$obj_file = "../scene/helix_0.obj";

#open OBJ, $obj_file;
while(<STDIN>) {
    next if (/^#/);
    next if (/^\s*$/);
    if(/^v\s+(\S+)\s+(\S+)\s+(\S+)\s*$/) {
	push @v, [$1+0.0, $2+0.0, $3+0.0];
    } elsif(/^vn\s+(\S+)\s+(\S+)\s+(\S+)\s*$/) {
	push @vn, [$1+0.0, $2+0.0, $3+0.0];
    } elsif(/^vt\s+(\S+)\s+(\S+)\s*$/) {
	push @vt, [$1+0.0, $2+0.0];
    } elsif(/^g\s+(.*)$/) {
	# G ?
    } elsif(/^s\s+(.*)$/) {
	# S ?
    } elsif(/^mtllib\s+(.*)$/) {
	# MTLLIB ?
	load_mtl($mtl_path.$1);
    } elsif(/^usemtl\s+(.*)$/) {
	# USEMTL ?
	#$cur_material = $1;
	$cur_material = 0;
    } elsif(/^f\s+(.*)$/) {
        # F v/vt/vn
	my @vertices = map { m!^(\d+)/(\d+)/(\d+)$! or die "Weird v format: $_"; add_if_needed_vtx($1, $2, $3) } split /\s+/, $1;
	add_poly($cur_material, @vertices);
    } elsif(/^fo\s+(.*)$/) {
	# fo
    } else {
	print "??? $_";
    }
}

print scalar(@out_vtx), " ", scalar(@out_tri), "\n";
print @out_vtx;
print @out_tri;

sub add_if_needed_vtx {
    my ($v_i, $vt_i, $vn_i) = @_;
    die "Vertex $v_i requested, but only 1..".(@v)." available" if $v_i > @v;
    die "Vertex-texcoords $vt_i requested, but only 1..".(@vt)." available" if $vt_i > @vt;
    die "Vertex-normal $vn_i requested, but only 1..".(@vn)." available" if $vn_i > @vn;
    my $v  = $v [$v_i -1];
    my $vt = $vt[$vt_i-1];
    my $vn = $vn[$vn_i-1];
    my $packed_v = pack "d8", $v->[0],  $v->[1],  $v->[2],
                              $vn->[0], $vn->[1], $vn->[2],
			      $vt->[0], $vt->[1];
    if(exists $cached_vtx{$packed_v}) {
	return $cached_vtx{$packed_v}
    }
    push @out_vtx, sprintf "%f %f %f %f %f %f %f %f\n",
                              $v->[0],  $v->[1],  $v->[2],
                              $vn->[0], $vn->[1], $vn->[2],
			      $vt->[0], $vt->[1];
    $cached_vtx{$packed_v} = $#out_vtx;
    return $cached_vtx{$packed_v};
}

sub load_mtl {
    my ($mtl_file) = @_;
}

sub add_poly {
    my ($material, @vertices) = @_;
    die ("Only ".(scalar @vertices)." vertices, 3+ needed") if @vertices < 3;
    while(@vertices > 3) {
	my $last_vertex = pop @vertices;
	add_triangle($material, $vertices[-1], $last_vertex, $vertices[0]);
    }
    add_triangle($material, @vertices);
}

sub add_triangle {
    my ($material, @vertices) = @_;
    my $packed_tri = pack "d", @vertices, $material;
    push @out_tri, sprintf "%d %d %d %d\n", @vertices, $material;
}
