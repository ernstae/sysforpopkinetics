

class Optimizer
{
public:
  // Sets inputs required by this particular optimizer.
  virtual void setInputs() = 0;

  // Calls the optimizer with its particular inputs.
  virtual void optimize() = 0;

  // Convert the optimizer's output to the appropriate form
  virtual outputReport() = 0;
}

// Class: QuasiNewton01BoxManager
//
//
// Manager for QuasiNewton01Box, which solves quadratic problems
// with box constraints using exact complementarity.
//
class QuasiNewton01BoxManager : Optimizer
{
public:
  // Sets inputs required by this particular optimizer.
  void setInputs()

    // Calls the optimizer with its particular inputs.
    void optimize()
  {
     msg = QuasiNewton01Box(
          // Input Arguments
          os,
          level,
          ItrMax,
          QuadMax,
          n,
          delta,
          obj,
          // Input+Output Arguments
          ItrCur,
          QuadCur,
          rCur,
          fCur,
          xCur,
          gCur,
          HCur );
  }

private:
     std::ostream    &os,
     size_t        level,
     size_t       ItrMax,
     size_t      QuadMax,
     size_t            n,
     double        delta,
     Fun          &obj,
     // Input+Output Arguments
     size_t      &ItrCur,
     size_t     &QuadCur,
     double        &rCur,
     double        &fCur,
     double        *xCur, // length n 
     double        *gCur, // length n 
     const double  *HCur  // length n * n )

}
