/* -*- mode: C++;-*- */
#define PERL_NO_GET_CONTEXT	/* we want efficiency */
#define __STDC_LIMIT_MACROS

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"

#undef seed

#include "constants.hpp"
#include <cstdint>
#include <algorithm>
#include <unordered_set>

using constants::BACKGROUND_CHAR;
using constants::BACKGROUND_CHAR_VISIBLE;
using constants::EMPTY_CHAR;
using constants::BLACK_CHAR;
using constants::WHITE_CHAR;
using constants::EDGE_CHAR;

typedef std::unordered_set<std::string> StringSet;

/* For some reason this does not get defined if C++ even though gcc supports it */
#undef PERL_UNUSED_DECL
#define PERL_UNUSED_DECL __attribute__unused__

/* Workaround for older perls without packWARN */
#ifndef packWARN
# define packWARN(a) (a)
#endif

/* Workaround for older perls without HvENAME */
#ifndef HvENAME
# define HvENAME(a)	HvNAME(a)
#endif /* HvENAME */

/* Workaround for older perls without packWARN */
#ifndef packWARN
# define packWARN(a) (a)
#endif

/* Workaround for older perls without HvENAME */
#ifndef HvENAME
# define HvENAME(a)	HvNAME(a)
#endif /* HvENAME */

/* Duplicate from perl source (since it's not exported unfortunately) */
STATIC bool my_isa_lookup(pTHX_ HV *stash, const char *name, HV* name_stash,
                          int len, int level) COLD;
STATIC bool my_isa_lookup(pTHX_ HV *stash, const char *name, HV* name_stash,
                          int len, int level) {
    AV* av;
    GV* gv;
    GV** gvp;
    HV* hv = Nullhv;
    SV* subgen = Nullsv;

    /* A stash/class can go by many names (ie. User == main::User), so
       we compare the stash itself just in case */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wnonnull"
    if ((name_stash && stash == name_stash) ||
        strEQ(HvENAME(stash), name) ||
        strEQ(name, "UNIVERSAL")) return TRUE;
#pragma GCC diagnostic pop

    if (level > 100) croak("Recursive inheritance detected in package '%s'",
                           HvENAME(stash));

    gvp = (GV**)hv_fetch(stash, "::ISA::CACHE::", 14, FALSE);

    if (gvp && (gv = *gvp) != (GV*)&PL_sv_undef && (subgen = GvSV(gv)) &&
        (hv = GvHV(gv))) {
        if (SvIV(subgen) == (IV)PL_sub_generation) {
            SV* sv;
            SV** svp = (SV**)hv_fetch(hv, name, len, FALSE);
            if (svp && (sv = *svp) != (SV*)&PL_sv_undef) {
                DEBUG_o( Perl_deb(aTHX_ "Using cached ISA %s for package %s\n",
                                  name, HvENAME(stash)) );
                return sv == &PL_sv_yes;
            }
        } else {
            DEBUG_o( Perl_deb(aTHX_ "ISA Cache in package %s is stale\n",
                              HvENAME(stash)) );
            hv_clear(hv);
            sv_setiv(subgen, PL_sub_generation);
        }
    }

    gvp = (GV**)hv_fetch(stash,"ISA",3,FALSE);

    if (gvp && (gv = *gvp) != (GV*)&PL_sv_undef && (av = GvAV(gv))) {
	if (!hv || !subgen) {
	    gvp = (GV**)hv_fetch(stash, "::ISA::CACHE::", 14, TRUE);

	    gv = *gvp;

	    if (SvTYPE(gv) != SVt_PVGV)
		gv_init(gv, stash, "::ISA::CACHE::", 14, TRUE);

	    if (!hv)
		hv = GvHVn(gv);
	    if (!subgen) {
		subgen = newSViv(PL_sub_generation);
		GvSV(gv) = subgen;
	    }
	}
	if (hv) {
	    SV** svp = AvARRAY(av);
	    /* NOTE: No support for tied ISA */
	    I32 items = AvFILLp(av) + 1;
	    while (items--) {
		SV* sv = *svp++;
		HV* basestash = gv_stashsv(sv, FALSE);
		if (!basestash) {
		    if (ckWARN(WARN_MISC))
			Perl_warner(aTHX_ packWARN(WARN_SYNTAX),
                                    "Can't locate package %" SVf " for @%s::ISA",
                                    sv, HvENAME(stash));
		    continue;
		}
		if (my_isa_lookup(aTHX_ basestash, name, name_stash,
                                  len, level + 1)) {
		    (void)hv_store(hv,name,len,&PL_sv_yes,0);
		    return TRUE;
		}
	    }
	    (void)hv_store(hv,name,len,&PL_sv_no,0);
	}
    }
    return FALSE;
}

#define C_SHAPE(object, context) c_Shape(aTHX_ object, context)

STATIC SV* c_Shape(pTHX_ SV *object, const char *context) COLD;
STATIC SV* c_Shape(pTHX_ SV *object, const char *context) {
    if (MAGIC) SvGETMAGIC(object);
    if (!SvROK(object)) {
        if (SvOK(object)) croak("%s is not a reference", context);
        croak("%s is undefined", context);
    }
    SV* sv = SvRV(object);
    if (!SvOBJECT(sv)) croak("%s is not an object reference", context);
    HV *stash = SvSTASH(sv);
    /* Is the next even possible ? */
    if (!stash) croak("%s is not a typed reference", context);
    HV *class_stash = gv_stashpv("Go::CountLiberties::Shape", FALSE);
    if (!my_isa_lookup(aTHX_ stash, "Go::CountLiberties::Shape", class_stash, 25, 0))
        croak("%s is not a Go::CountLiberties::Shape reference", context);
    return sv;
}

#define C_STRINGSET(object, context) c_StringSet(aTHX_ object, context)

STATIC StringSet* c_StringSet(pTHX_ SV *object, const char *context) COLD;
STATIC StringSet* c_StringSet(pTHX_ SV *object, const char *context) {
    if (MAGIC) SvGETMAGIC(object);
    if (!SvROK(object)) {
        if (SvOK(object)) croak("%s is not a reference", context);
        croak("%s is undefined", context);
    }
    SV* sv = SvRV(object);
    if (!SvOBJECT(sv)) croak("%s is not an object reference", context);
    HV *stash = SvSTASH(sv);
    /* Is the next even possible ? */
    if (!stash) croak("%s is not a typed reference", context);
    HV *class_stash = gv_stashpv("Go::CountLiberties::StringSet", FALSE);
    if (!my_isa_lookup(aTHX_ stash, "Go::CountLiberties::StringSet", class_stash, 29, 0))
        croak("%s is not a Go::CountLiberties::StringSet reference", context);
    IV address = SvIV(sv);
    if (!address)
        croak("Go::CountLiberties::StringSet object %s has a NULL pointer", context);
    return INT2PTR(StringSet *, address);
}

class Vertex;
class Vertex {
  private:
    uint16_t vertex_;

    static uint16_t const negative_xor[constants::COLORS];

  public:
    enum {
        MAX	   =  63,
        MIN	   = -64,
        OFFSET	   =  64,
        X_MASK     = 0x01fc,
        Y_MASK     = 0xfe00,
        COLOR_MASK = 0x0003,
        X_ADD      = 1 << 2,
        Y_ADD      = 1 << 9,
    };
    // This liberty stuff should go into its own class
    // Allows us to have a proper constructor (to replace init_static)
    // Would also allow us to have one such object per thread instead of global
    static bool     liberty_matrix_[0x4000];
    static uint16_t liberties_[0x4000];
    static int liberty_count_;

    Vertex() {}
    Vertex(int x, int y, int color = constants::COLOR_MAX) :
    vertex_{(uint16_t) (((y+OFFSET) << 9) + ((x+OFFSET) << 2) + color)} {
    }
    Vertex(Vertex const& from) : vertex_{from.vertex_} {}
    explicit Vertex(uint16_t vertex) : vertex_{vertex} {}
    explicit operator int() { return vertex_; }
    static void init_static();
    void expand(int &x, int &y, int &color) const {
        y = (int)  (vertex_ >> 9) - OFFSET;;
        x = (int) ((vertex_ >> 2) & 0x7f) - OFFSET;
        color = vertex_ & constants::COLOR_MAX;
    }
    void expand(int &x, int &y) const {
        y = (int) (vertex_ >> 9) - OFFSET;
        x = (int) ((vertex_ >> 2) & 0x7f) - OFFSET;
    }
    uint8_t _get_x() const { return (vertex_ >> 2) & 0x7f; }
    uint8_t _get_y() const { return  vertex_ >> 9; }
    void _get_expand(int &x, int &y, int &color) const {
        y =  vertex_ >> 9;
        x = (vertex_ >> 2) & 0x7f;
        color = vertex_ & constants::COLOR_MAX;
    }
    void _get_expand(int &x, int &y) const {
        y =  vertex_ >> 9;
        x = (vertex_ >> 2) & 0x7f;
    }
    static Vertex _get_compress(int x, int y, int color = constants::COLOR_MAX) {
        Vertex result;
        result.vertex_ = (uint16_t) ((y << 9) + (x << 2) + color);
        return result;
    }
    uint get_color() const { return vertex_ & constants::COLOR_MAX; }
    void color_up() { vertex_ |= constants::COLOR_MAX; }
    void negate() {
        vertex_ ^= negative_xor[vertex_ & constants::COLOR_MAX];
    }
    Vertex mirror_x() const {
        //dump("before mirror");
        Vertex result(*this);
        result.vertex_ = (result.vertex_ ^ X_MASK) + X_ADD;
        return result;
    }
    Vertex mirror_y() const {
        Vertex result(*this);
        result.vertex_ = (result.vertex_ ^ Y_MASK) + Y_ADD;
        return result;
    }
    Vertex different() const {
        Vertex result;
        result.vertex_ = ~ this->vertex_;
        return result;
    }
    uint16_t y_masked() { return vertex_ & Y_MASK; }
    uint16_t x_masked() { return vertex_ & X_MASK; }
    void rotate_left() {
        int x, y, color;
        _get_expand(x, y, color);
        *this = _get_compress((y ^ 0x7f)+1, x, color);
    }
    void _get_translate(Vertex offset) {
        vertex_ += offset.vertex_;
    }
    void _get_unset_liberty() {
        liberty_matrix_[vertex_ >> 2] =
            (vertex_ & COLOR_MASK) == constants::EMPTY;
    }
    void _set_liberty() {
        liberty_matrix_[vertex_ >> 2] = 1;
    }
    void _get_liberties(int from_color) {
        if ((vertex_ & COLOR_MASK) != from_color) return;

        uint16_t base = vertex_ >> 2;

        --base;
        liberties_[liberty_count_] = base;
        liberty_count_ += liberty_matrix_[base];
        liberty_matrix_[base] = 0;

        base += 2;
        liberties_[liberty_count_] = base;
        liberty_count_ += liberty_matrix_[base];
        liberty_matrix_[base] = 0;

        base += 128-1;
        liberties_[liberty_count_] = base;
        liberty_count_ += liberty_matrix_[base];
        liberty_matrix_[base] = 0;

        base -= 256;
        liberties_[liberty_count_] = base;
        liberty_count_ += liberty_matrix_[base];
        liberty_matrix_[base] = 0;
    }
    static void _set_nr_liberties() {
        for (int i=0; i<liberty_count_; i++) {
            uint16_t base = liberties_[i];
            liberty_matrix_[base] = 1;
        }
        liberty_count_ = 0;
    }
    static int liberty_count() { return liberty_count_; }
    static Vertex _get_liberty_result(int i, int color) {
        Vertex result;
        result.vertex_ = (liberties_[i] << 2) + color;
        return result;
    }
    bool same_position(Vertex const& to) {
        return (vertex_ | constants::COLOR_MAX) == (to.vertex_ | constants::COLOR_MAX);
    }
    Vertex& operator=(Vertex const& from) {
        vertex_ = from.vertex_;
        return *this;
    }
    friend bool operator==(Vertex const& lhs, Vertex const& rhs) {
        return lhs.vertex_ == rhs.vertex_;
    }
    friend bool operator!=(Vertex const& lhs, Vertex const& rhs) {
        return lhs.vertex_!= rhs.vertex_;
    }
    friend bool operator<(Vertex const& lhs, Vertex const& rhs) {
        return lhs.vertex_ > rhs.vertex_;
    }
    friend bool operator<=(Vertex const& lhs, Vertex const& rhs) {
        return lhs.vertex_ >= rhs.vertex_;
    }
    friend bool operator>(Vertex lhs, Vertex rhs) {
        return lhs.vertex_ < rhs.vertex_;
    }
    friend bool operator>=(Vertex const& lhs, Vertex const& rhs) {
        return lhs.vertex_ <= rhs.vertex_;
    }
    void dump(const char *context) {
        int x, y, color;
        expand(x, y, color);
        warn("%s%s[%d,%d] -> %d (%04x)\n",
             context ? context : "",
             context ? ": " : "",
             x, y, color, vertex_);
    }
    void _get_dump(const char *context) {
        int x, y, color;
        _get_expand(x, y, color);
        warn("%s%s[%d,%d] -> %d (%04x)\n",
             context ? context : "",
             context ? ": " : "",
             x, y, color, vertex_);
    }
};

void Vertex::init_static() {
    for (int i=0; i<(int) (sizeof(liberty_matrix_)/sizeof(liberty_matrix_[0])); i++)
        liberty_matrix_[i] = 1;
}
uint16_t const Vertex::negative_xor[constants::COLORS] = {
    0,
    constants::BLACK ^ constants::WHITE,
    constants::BLACK ^ constants::WHITE,
    0,
};
bool Vertex::liberty_matrix_[0x4000];
uint16_t Vertex::liberties_[0x4000];
int Vertex::liberty_count_ = 0;

class Shape {
  private:
    Vertex data_[1];

  public:
    Vertex const& operator[](int i) const { return data_[i]; }
    Vertex & operator[](int i) { return data_[i]; }
    Vertex& find(int nr, Vertex target) {
        target.color_up();
        // probably clearer to replace the following code by std::lower_bound

        // Binary search for first >=.
        // Remember that internally we order from large to small since we
        // reversed the order operators
        Vertex* from = data_;
        Vertex* to   = &from[nr];
        // if inside the range the invariant is: from[-1] < target <= to[0]
        // (imagine data[-1] = data[0]+1 and data[len] = data[len-1]-1
        while (from < to) {
            int step = (to-from) / 2;
            if (from[step] < target) from += step+1;
            else to = from+step;
        }
        return *from;
    }
    static int nr(STRLEN len) {
        return (len - (sizeof(Shape)-sizeof(Vertex))) / sizeof(Vertex);
    }
    static Shape& FromPV(pTHX_ SV* pv, int& nr) {
        STRLEN len;
        char *data = SvPV(pv, len);
        nr = Shape::nr(len);
        return *(Shape *) data;
    }
    static Shape& FromPV(pTHX_ SV* pv) { return *(Shape *) SvPV_nolen(pv); }
    static Shape& FromObject(pTHX_ SV* rv, const char *context, int& nr) {
        SV* pv = C_SHAPE(rv, context);
        return FromPV(aTHX_ pv, nr);
    }
    static Shape& FromObject(pTHX_ SV* rv, const char *context) {
        SV* pv = C_SHAPE(rv, context);
        return FromPV(aTHX_ pv);
    }
    static Shape& grow(pTHX_ SV* pv, int nr) {
        char *data =
            SvGROW(pv, sizeof(Shape)-sizeof(Vertex)+1 + nr * sizeof(Vertex));
        return *(Shape *) data;
    }

    void _get_bounding_box(int nr,
                       uint8_t &x_min, uint8_t &y_min,
                       uint8_t &x_max, uint8_t &y_max) {
        if (nr) {
            y_min = data_[nr-1]._get_y();
            y_max = data_[0]._get_y();
            x_min = 0xff;
            x_max = 0;
            for (int i=0; i<nr; i++) {
                int x = data_[i]._get_x();
                if (x < x_min) x_min = x;
                if (x > x_max) x_max = x;
            }
        } else {
            x_min = 0xff;
            x_max = 0;
            y_min = 0xff;
            y_max = 0;
        }
    }

    void bounding_box(int nr,
                      int &x_min, int &y_min,
                      int &x_max, int &y_max) {
        uint8_t x_min8, y_min8, x_max8, y_max8;
        _get_bounding_box(nr, x_min8, y_min8, x_max8, y_max8);
        x_min = x_min8 - Vertex::OFFSET;
        y_min = y_min8 - Vertex::OFFSET;
        x_max = x_max8 - Vertex::OFFSET;
        y_max = y_max8 - Vertex::OFFSET;
    }

    void sort(int nr) { std::sort(data_, data_+nr); }
    void stable_sort(int nr) { std::stable_sort(data_, data_+nr); }
    // _unique assumes an extra work vertex at the end of the range!!!
    int _get_unique(int nr) {
        if (nr <= 1) return nr;
        data_[nr] = data_[nr-1].different();
        for (int i=0; i<nr-1; i++) {
            if (data_[i].same_position(data_[i+1])) {
                int k = i;
                int j = i+1;
                do {
                    while (data_[i].same_position(data_[j+1])) j++;
                    data_[k] = data_[j];
                    ++k;
                    i = ++j;
                } while (i < nr);
                return k;
            }
        }
        return nr;
    }
    void mirror_x(int nr);
    void rotate_left(int nr);
    void translate(int nr, int x, int y);
    int liberties(int nr, int color);
    void liberties_result(int nr, int color);
    static Vertex liberty_result(int i, int color);
};

void Shape::mirror_x(int nr) {
    int i=0;
    while (i < nr) {
        uint16_t y_cur = data_[i].y_masked();
        int j = i;
        while (++i < nr && data_[i].y_masked() == y_cur);
        // i now points one beyond the values with the same y as j
        int k = i-1;
        while (j < k) {
            Vertex tmp = data_[j].mirror_x();
            data_[j] = data_[k].mirror_x();
            data_[k] = tmp;
            ++j;
            --k;
        }
        if (j == k)
            data_[j] = data_[j].mirror_x();
    }
}

void Shape::rotate_left(int nr) {
    for (int i=0; i<nr; i++)
        data_[i].rotate_left();
    sort(nr);
}

void Shape::translate(int nr, int x, int y) {
    if (nr == 0) return;
    if (x == 0 && y == 0) return;

    /*
    uint8_t x_min, x_max, y_min, y_max;
    _get_bounding_box(nr, x_min, y_min, x_max, y_max);
    if (x_min + x < Vertex::MIN) throw;
    if (x_max + x > Vertex::MAX) throw;
    if (y_min + y < Vertex::MIN) throw;
    if (y_max + y > Vertex::MAX) throw;
    */

    Vertex offset = Vertex::_get_compress(x, y, 0);
    for (int i=0; i<nr; i++)
        data_[i]._get_translate(offset);
}

int Shape::liberties(int nr, int color) {
    // Clean up any mess a previous call left
    Vertex::_set_nr_liberties();
    // Occupied vertices do not lead to a liberty
    for (int i=0; i<nr; i++)
        data_[i]._get_unset_liberty();
    // Consider all vertices around ours
    for (int i=0; i<nr; i++)
        data_[i]._get_liberties(color);
    // Already clean up our own vertices
    // (our libberties will be done at the next call
    for (int i=0; i<nr; i++)
        data_[i]._set_liberty();
    return Vertex::liberty_count();
}

void Shape::liberties_result(int nr, int color) {

    for (int i=0; i < nr; i++)
        data_[i] = Vertex::_get_liberty_result(i, color);
    sort(nr);
}

Vertex Shape::liberty_result(int i, int color) {
    return Vertex::_get_liberty_result(i, color);
}

#define PEEK(sv)	peek(aTHX_ __LINE__, (sv))
void peek(pTHX_ int line, SV* sv) {
    PerlIO_printf(Perl_debug_log, "Dump SV at line %d:\n", line);
    do_sv_dump(0, Perl_debug_log, sv, 0, 25, 0, 400);
}

STATIC void initialize(void) COLD;
STATIC void initialize(void) {
    Vertex::init_static();
}

MODULE = Go::CountLiberties::Shape		PACKAGE = Go::CountLiberties::Shape
PROTOTYPES: DISABLE

SV *
new(char *class_name)
  CODE:
    RETVAL = sv_newmortal();
    newSVrv(RETVAL, class_name);
    SV *pv = SvRV(RETVAL);
    sv_setpvn(pv, "", 0);
    SvREFCNT_inc(RETVAL);
  OUTPUT:
    RETVAL

void
get(SV *rv, int x, int y, unsigned int dummy_color=0)
  PPCODE:
    PERL_UNUSED_VAR(dummy_color);
    if (x > Vertex::MAX) croak("x value %d too large", x);
    if (x < Vertex::MIN) croak("x value %d too small", x);
    if (y > Vertex::MAX) croak("y value %d too large", y);
    if (y < Vertex::MIN) croak("y value %d too small", y);
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    // EXTEND(SP, 1);
    if (nr == 0)
        PUSHs(&PL_sv_undef);
    else {
        Vertex target(x, y);
        Vertex& from = shape.find(nr, target);
        if (&from < &shape[nr] && from.same_position(target)) {
            dXSTARG;
            PUSHu(from.get_color());
        } else
            PUSHs(&PL_sv_undef);
    }

void
set(SV *rv, int x, int y, unsigned int color)
  PPCODE:
    if (color > constants::COLOR_MAX) croak("Unknown color %u", color);
    if (x > Vertex::MAX) croak("x value %d too large", x);
    if (x < Vertex::MIN) croak("x value %d too small", x);
    if (y > Vertex::MAX) croak("y value %d too large", y);
    if (y < Vertex::MIN) croak("y value %d too small", y);
    Vertex target(x, y, color);
    SV* pv = C_SHAPE(rv, "shape");
    STRLEN len = SvCUR(pv);
    if (len == 0) {
        // Grow it for at least 9 entries, also gets us the content
        Shape& shape = Shape::grow(aTHX_ pv, 9);
        shape[0] = target;
        SvCUR_set(pv, sizeof(Shape));
        *SvEND(pv) = '\0';
    } else {
        // Get data and make sure there is space for one more in case we extend
        // Ensure the shape/result of find won't get invalidated be a later grow
        int nr = Shape::nr(len);
        Shape& shape = Shape::grow(aTHX_ pv, nr+1);
        Vertex& from = shape.find(nr, target);
        if (&from == &shape[nr]) {
            from = target;
            SvCUR_set(pv, len+sizeof(Vertex));
            *SvEND(pv) = 0;
        } else if (from.same_position(target))
            from = target;
        else {
            Move(&from, &from+1, (char *)&shape +len+1-(char *) &from, char);
            from = target;
            SvCUR_set(pv, len+sizeof(Vertex));
            *SvEND(pv) = 0;
        }
    }
    PUSHs(rv);

void
_set(SV *rv, int value, unsigned int color=0)
  PPCODE:
    if (value < 0) croak("Value %d too small", value);
    if (value >= 0x10000) croak("Value %d too large", value);
    int x, y, c;
    Vertex(value).expand(x, y, c);
    if (items >= 3) {
        if (color > constants::COLOR_MAX) croak("Unknown color %u", color);
        c = color;
    }
    if (x > Vertex::MAX) croak("x value %d too large", x);
    if (x < Vertex::MIN) croak("x value %d too small", x);
    if (y > Vertex::MAX) croak("y value %d too large", y);
    if (y < Vertex::MIN) croak("y value %d too small", y);
    Vertex target(x, y, c);
    SV* pv = C_SHAPE(rv, "shape");
    STRLEN len = SvCUR(pv);
    if (len == 0) {
        // Grow it for at least 9 entries, also gets us the data
        Shape& shape = Shape::grow(aTHX_ pv, 9);
        shape[0] = target;
        SvCUR_set(pv, sizeof(Shape));
        *SvEND(pv) = '\0';
    } else {
        // Get data and make sure there is space for one more in case we extend
        // Ensure the shape/result of find won't get invalidated be a later grow
        int nr = Shape::nr(len);
        Shape& shape = Shape::grow(aTHX_ pv, nr+1);
        Vertex& from = shape.find(nr, target);
        if (&from == &shape[nr]) {
            from = target;
            SvCUR_set(pv, len+sizeof(Vertex));
            *SvEND(pv) = 0;
        } else if (from.same_position(target))
            from = target;
        else {
            Move(&from, &from+1, (char *) &shape+len+1-(char *) &from, char);
            from = target;
            SvCUR_set(pv, len+sizeof(Vertex));
            *SvEND(pv) = 0;
        }
    }
    PUSHs(rv);

void
set_from_id(SV *rv, SV *from_pv)
  PPCODE:
    STRLEN len;
    char *from_data = SvPV(from_pv, len);
    if (len != 0) {
        int nr = len / sizeof(Vertex);
        if (len != nr * sizeof(Vertex))
            croak("Invalid id length %lu", (ulong) len);
        SV* to_pv = C_SHAPE(rv, "shape");
        int i = Shape::nr(SvCUR(to_pv));
        int old_i = i;
        // Add 1 for the _unique terminator and as target for at least 1 set
        Shape& shape = Shape::grow(aTHX_ to_pv, nr+i+1);
        Vertex* from_vertex = (Vertex *) from_data;
        while (nr) {
            shape[i] = *from_vertex;
            ++from_vertex;
            ++i;
            --nr;
        }
        if (old_i) {
            shape.stable_sort(i);
            i = shape._get_unique(i);
        } else
            shape.sort(i);

        char *end = (char *) &shape[i];
        *end = 0;
        SvCUR_set(to_pv, end - (char *) &shape);
    }
    PUSHs(rv);

SV *
id(SV *rv)
  CODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    if (nr == 0) {
        RETVAL = newSVpvn("", 0);
    } else {
        uint8_t x_min, x_max, y_min, y_max;
        shape._get_bounding_box(nr, x_min, y_min, x_max, y_max);
        int x_offset = Vertex::OFFSET - (int)(x_max+x_min+1)/2;
        int y_offset = Vertex::OFFSET - (int)(y_max+y_min+1)/2;

        RETVAL = newSV(nr * sizeof(Vertex));
        SvPOK_on(RETVAL);
        char* data = SvPV_nolen(RETVAL);

        SvCUR_set(RETVAL, nr * sizeof(Vertex));
        Vertex *to = (Vertex *) data;
        for (int i=0; i<nr; i++) {
            int x, y, color;
            shape[i]._get_expand(x, y, color);
            x += x_offset;
            y += y_offset;
            *to++ = Vertex::_get_compress(x, y, color);
        }
        *(char*) to = 0;
    }
  OUTPUT:
    RETVAL

SV *
as_string(SV *rv, char background = BACKGROUND_CHAR_VISIBLE, char empty = EMPTY_CHAR, char black = BLACK_CHAR, char white = WHITE_CHAR, char edge = EDGE_CHAR)
  CODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    if (nr == 0) {
        RETVAL = newSVpvn("", 0);
    } else {
        uint8_t x_min, x_max, y_min, y_max;
        shape._get_bounding_box(nr, x_min, y_min, x_max, y_max);

        uint x_row = x_max-x_min+2u;
        uint area = x_row * (y_max-y_min+1u);
        RETVAL = newSV(area);
        SvPOK_on(RETVAL);
        char* data = SvPV_nolen(RETVAL);
        char *ptr = data;
        for (uint x=1; x<x_row; ++x) *ptr++ = background;
        *ptr++ = '\n';
        for (uint y = y_max-y_min; y>0; --y) {
            Copy(data, ptr, x_row, char);
            ptr += x_row;
        }

        for (int i=0; i<nr; i++) {
            int x, y, color;
            shape[i]._get_expand(x, y, color);
            if (x < x_min || x > x_max || y < y_min || y > y_max)
                croak("Bad bounding_box");
            data[(y_max-y)*x_row+(x-x_min)] =
                color == constants::EMPTY ? empty :
                color == constants::BLACK ? black :
                color == constants::WHITE ? white :
                color == constants::EDGE  ? edge  :
                '?';
        }
        data[area] = 0;
        SvCUR_set(RETVAL, area);
    }
  OUTPUT:
    RETVAL

void
bounding_box(SV *rv)
  PPCODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    EXTEND(SP, 4);
    if (nr == 0) {
        PUSHs(&PL_sv_undef);
        PUSHs(&PL_sv_undef);
        PUSHs(&PL_sv_undef);
        PUSHs(&PL_sv_undef);
    } else {
        int x_min, x_max, y_min, y_max;
        shape.bounding_box(nr, x_min, y_min, x_max, y_max);
        mPUSHi(x_min);
        mPUSHi(y_min);
        mPUSHi(x_max);
        mPUSHi(y_max);
    }

SV *
clone(SV *rv)
  CODE:
    SV* pv_from = C_SHAPE(rv, "shape");

    RETVAL = sv_newmortal();
    newSVrv(RETVAL, "Go::CountLiberties::Shape");
    SV *pv_to = SvRV(RETVAL);
    sv_copypv(pv_to, pv_from);
    SvREFCNT_inc(RETVAL);
  OUTPUT:
    RETVAL

void
negate(SV *rv)
  PPCODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    for (int i=0; i<nr; ++i)
        shape[i].negate();
    PUSHs(rv);

void
mirror_x(SV *rv)
  PPCODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    shape.mirror_x(nr);
    PUSHs(rv);

void
rotate_left(SV *rv)
  PPCODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    shape.rotate_left(nr);
    PUSHs(rv);

void
translate(SV* rv, int x, int y)
  PPCODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    shape.translate(nr, x, y);
    PUSHs(rv);

SV *
liberties(SV *rv, unsigned int color)
  CODE:
    int nr;
    Shape& from_shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    RETVAL = sv_newmortal();
    newSVrv(RETVAL, "Go::CountLiberties::Shape");
    SV *pv = SvRV(RETVAL);
    if (nr == 0)
        sv_setpvn(pv, "", 0);
    else {
        nr = from_shape.liberties(nr, color);
        STRLEN len = sizeof(Shape) + (nr-1) * sizeof(Vertex);
        SvGROW(pv, len+1);
        sv_setpvn(pv, "", 0);
        char *data = SvPV_nolen(pv);
        Shape& to_shape = *(Shape *) data;
        to_shape.liberties_result(nr, color);
        SvCUR_set(pv, len);
        *SvEND(pv) = 0;
    }
    SvREFCNT_inc(RETVAL);
  OUTPUT:
    RETVAL

void
orthogonal(SV *rv, int x, int y, unsigned int color = constants::BLACK)
  ALIAS:
    Go::CountLiberties::Shape::neighbours = 1
  PPCODE:
    if (color > constants::COLOR_MAX) croak("Unknown color %u", color);
    if (x > Vertex::MAX-1) croak("x value %d too large", x);
    if (x < Vertex::MIN+1) croak("x value %d too small", x);
    if (y > Vertex::MAX-1) croak("y value %d too large", y);
    if (y < Vertex::MIN+1) croak("y value %d too small", y);
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    EXTEND(SP, 4);
    auto check_direction = [&](int x, int y) {
        unsigned int c = constants::COLOR_MAX+1;
        if (nr) {
            Vertex target(x, y);
            Vertex& from = shape.find(nr, target);
            if (&from < &shape[nr] && from.same_position(target))
                c = from.get_color();
        }
        if (ix == 0 || c == color) {
            AV* av = newAV();
            av_extend(av, 3);
            av_push(av, newSViv(x));
            av_push(av, newSViv(y));
            av_push(av, c == constants::COLOR_MAX+1 ? &PL_sv_undef : newSViv(c));
            SV* rv = newRV_noinc((SV *) av);
            sv_2mortal(rv);
            PUSHs(rv);
        }
    };
    check_direction(x+1, y);
    check_direction(x  , y+1);
    check_direction(x-1, y);
    check_direction(x  , y-1);

void
_liberties(SV *rv, int color)
  PPCODE:
    int nr;
    Shape& from_shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    if (GIMME_V == G_ARRAY) {
        if (nr) {
            nr = from_shape.liberties(nr, color);
            EXTEND(SP, nr);
            for (int i=0; i<nr; i++)
                PUSHs(sv_2mortal(newSViv((int) Shape::liberty_result(i, color))));
        }
    } else if (GIMME_V == G_SCALAR) {
        if (nr) nr = from_shape.liberties(nr, color);
        dXSTARG;
        PUSHi(nr);
    }

void
keys(SV *rv)
  PPCODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    if (GIMME_V == G_ARRAY) {
        EXTEND(SP, nr);
        for (int i=0; i<nr; i++) {
            int x, y, color;
            shape[i].expand(x, y, color);
            AV *av = newAV();
            av_extend(av, 2);
            av_push(av, newSViv(x));
            av_push(av, newSViv(y));
            av_push(av, newSViv(color));
            SV *rv = newRV_noinc((SV *) av);
            sv_2mortal(rv);
            PUSHs(rv);
        }
    } else if (GIMME_V == G_SCALAR) {
        dXSTARG;
        PUSHi(nr);
    }

void
_keys(SV *rv)
  PPCODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    if (GIMME_V == G_ARRAY) {
        EXTEND(SP, nr);
        for (int i=0; i<nr; i++)
            PUSHs(sv_2mortal(newSViv((int) shape[i])));
    } else if (GIMME_V == G_SCALAR) {
        dXSTARG;
        PUSHi(nr);
    }

void
values(SV *rv)
  PPCODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    if (GIMME_V == G_ARRAY) {
        EXTEND(SP, nr);
        for (int i=0; i<nr; i++)
            PUSHs(sv_2mortal(newSViv(shape[i].get_color())));
    } else if (GIMME_V == G_SCALAR) {
        dXSTARG;
        PUSHi(nr);
    }

SV *
_internal(SV *rv)
  CODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    uint8_t x_min, x_max, y_min, y_max;
    shape._get_bounding_box(nr, x_min, y_min, x_max, y_max);

    RETVAL = newSV(13*nr+50);
    sv_setpvn(RETVAL, "", 0);
    sv_catpvf(RETVAL, "Box [%u,%u] [%u,%u], %d entries\n",
               x_min, y_min, x_max, y_max, nr);
    for (int i=0; i<nr; i++) {
        int x, y, color;
        shape[i]._get_expand(x, y, color);
        sv_catpvf(RETVAL, "  %2d,%2d -> %d\n", x, y, color);
    }
  OUTPUT:
    RETVAL

SV *
internal(SV *rv)
  CODE:
    int nr;
    Shape& shape = Shape::FromObject(aTHX_ rv, "shape", nr);
    int x_min, x_max, y_min, y_max;
    shape.bounding_box(nr, x_min, y_min, x_max, y_max);

    RETVAL = newSV(13*nr+50);
    sv_setpvn(RETVAL, "", 0);
    sv_catpvf(RETVAL, "Box [%d,%d] [%d,%d], %d entries\n",
               x_min, y_min, x_max, y_max, nr);
    for (int i=0; i<nr; i++) {
        int x, y, color;
        shape[i].expand(x, y, color);
        sv_catpvf(RETVAL, "  %2d,%2d -> %d\n", x, y, color);
    }
  OUTPUT:
    RETVAL

MODULE = Go::CountLiberties::Shape		PACKAGE = Go::CountLiberties::StringSet

SV *
new(char *class_name)
  CODE:
    RETVAL = sv_newmortal();
    StringSet* set = new StringSet;
    sv_setref_pv(RETVAL, class_name, set);
    SvREFCNT_inc(RETVAL);
  OUTPUT:
    RETVAL

void
StringSet::store(SV *pv)
  PPCODE:
    STRLEN len;
    const char* data = SvPV(pv, len);
    THIS->emplace(data, len);

bool
StringSet::find(SV *pv)
  CODE:
    STRLEN len;
    const char* data = SvPV(pv, len);
    // We really should put this static in class together with StringSet
    // but all this is not meant to be threadsafe anyways
    static std::string needle;
    needle.assign(data, len);
    auto it = THIS->find(needle);
    RETVAL = it != THIS->end();
  OUTPUT:
    RETVAL

void
StringSet::clear()

void
StringSet::keys()
  PPCODE:
    StringSet::size_type size = THIS->size();
    if (GIMME_V == G_ARRAY) {
        EXTEND(SP, size);
        for (std::string const& str: *THIS) {
            SV* pv = newSV(str.length()+1);
            sv_setpvn(pv, str.c_str(), str.length());
            PUSHs(sv_2mortal(pv));
        }
    } else if (GIMME_V == G_SCALAR) {
        dXSTARG;
        PUSHu(size);
    }

void
StringSet::keys_to_array(SV* rv)
  PPCODE:
    if (!SvROK(rv)) croak("Not a reference");
    AV* av = (AV*) SvRV(rv);
    if (SvTYPE(av) != SVt_PVAV) croak("Not an ARRAY reference");

    StringSet::size_type size = THIS->size();
    av_extend(av, size-1);
    for (std::string const& str: *THIS) {
        SV* pv = newSV(str.length()+1);
        sv_setpvn(pv, str.c_str(), str.length());
        av_push(av, pv);
    }

void
StringSet::DESTROY()

BOOT:
  initialize();
