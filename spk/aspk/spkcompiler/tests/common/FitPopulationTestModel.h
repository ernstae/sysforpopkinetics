#ifndef FITPOPULATIONTESTMODEL_H
#define FITPOPULATIONTESTMODEL_H

#include <spk/SpkValarray.h>
#include <spk/SpkModel.h>

/*************************************************************************
 *
 * Class: FitPopulationTestModel.h
 *
 *************************************************************************/

class FitPopulationTestModel : public SpkModel
{
public:
  FitPopulationTestModel( int, int, SPK_VA::valarray<int>& );
  ~FitPopulationTestModel();

protected:
  FitPopulationTestModel();
  FitPopulationTestModel( const FitPopulationTestModel& right );
  FitPopulationTestModel& operator=( const FitPopulationTestModel& right );
private:
  void doSelectIndividual        ( int whoIn );
  void doSetPopPar               ( const SPK_VA::valarray<double>& alpIn );
  void doSetIndPar               ( const SPK_VA::valarray<double>& bIn );
  void doIndParVariance          ( SPK_VA::valarray<double>& DOut )         const;
  bool doIndParVariance_popPar   ( SPK_VA::valarray<double>& D_alpOut )     const;
  void doIndParVarianceInv       ( SPK_VA::valarray<double>& DInvOut )      const;
  bool doIndParVarianceInv_popPar( SPK_VA::valarray<double>& DInv_alpOut )  const;
  void doDataMean                ( SPK_VA::valarray<double>& fiOut )        const;
  bool doDataMean_popPar         ( SPK_VA::valarray<double>& fi_alpOut )    const;
  bool doDataMean_indPar         ( SPK_VA::valarray<double>& fi_bOut )      const;
  void doDataVariance            ( SPK_VA::valarray<double>& RiOut )        const;
  bool doDataVariance_popPar     ( SPK_VA::valarray<double>& Ri_alpOut )    const;
  bool doDataVariance_indPar     ( SPK_VA::valarray<double>& Ri_bOut )      const;
  void doDataVarianceInv         ( SPK_VA::valarray<double>& RiInvOut )     const;
  bool doDataVarianceInv_popPar  ( SPK_VA::valarray<double>& RiInv_alpOut ) const;
  bool doDataVarianceInv_indPar  ( SPK_VA::valarray<double>& RiInv_bOut )   const;

  SPK_VA::valarray<double> alp, b;
  int who;
  SPK_VA::valarray<int> N;

};

#endif
