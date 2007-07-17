## AXIS.pm MODULE
## PURPOSE: IS TO HANDLE SUPPLYING AXISI AND AXISII LISTS
## AND FUNCTIONS FOR OBTAINING VALUES TO KEYS FROM THE LISTS
##
## Michael Riffle <mriffle@u.washington.edu>
## CREATED: 2002-06-22

## SET UP THE AXISI AND AXIS II ARRAYS
## WE'RE USING ARRAYS INSTEAD OF HASHES, BECAUSE THE ORDER OF THESE
## KEYS DOES MATTER, AND THE KEYS AND VALUES ARE NOT NECESSARILY IN A
## SORTABLE ORDER

package YRC::PROJECT::AXIS;
use strict;

my @AXISI = (
		[ '1A',		'Animals, Whole -> Vertebrates, Mammals' ],
		[ '1B',		'Animals, Whole -> Vertebrates, Non-Mammal' ],
		[ '1C',		'Animals, Whole -> Invertebrates' ],
		[ '1D', 	'Animals, Cell/Membrane/Tissue/Organ -> Vertebrates, Mammal' ],
		[ '1E', 	'Animals, Cell/Membrane/Tissue/Organ -> Vertebrates, Non-Mammal' ],
		[ '1F', 	'Animals, Cell/Membrane/Tissue/Organ -> Invertebrates' ],
		[ '2',		'Biological/Chemical Compounds' ],
		[ '3',		'Biomaterials' ],
		[ '4',		'Human, Cells Only' ],
		[ '5A',		'Human, Adult -> Female' ],
		[ '5B',		'Human, Adult -> Male' ],
		[ '5A',		'Human, Infant/Child -> Female' ],
		[ '5B',		'Human, Infant/Child -> Male' ],
		[ '6',		'Human, Membrane/Tissue/Isolated Organ' ],
		[ '7A', 	'Microorganisms -> Bacteria' ],
		[ '7B', 	'Microorganisms -> Viruses' ],
		[ '7C', 	'Microorganisms -> Parasites' ],
		[ '7D', 	'Microorganisms -> Other' ],
		[ '8',		'Plants/Fungi' ],
		[ '9',		'Technology/Technique Development' ],
		[ '11',		'Facility Construction/Improvement' ],
		[ '12A',	'Clinical Trials -> Multi-center' ],
		[ '12B',	'Clinical Trials -> Single Center' ],
		[ '13',		'Cardiovascular System' ],
		[ '14',		'Connective Tissue' ],
		[ '15',		'Endocrine System' ],
		[ '16A',	'Gastrointestinal System -> Esophagus' ],
		[ '16B',	'Gastrointestinal System -> Gallbladder' ],
		[ '16C',	'Gastrointestinal System -> Intestine' ],
		[ '16D',	'Gastrointestinal System -> Liver' ],
		[ '16E',	'Gastrointestinal System -> Pancreas' ],
		[ '16F',	'Gastrointestinal System -> Stomach' ],
		[ '17',		'Hematologic System' ],
		[ '18',		'Integumentary/Skin System' ],
		[ '19',		'Lymphatic and Recticulo-Edothelial System' ],
		[ '20',		'Muscular System' ],
		[ '21',		'Nervous System' ],
		[ '22',		'Oral/Dental' ],
		[ '23',		'Reproductive System' ],
		[ '24',		'Respiratory System' ],
		[ '25A',	'Sensory System -> Ear' ],
		[ '25B',	'Sensory System -> Eye' ],
		[ '25C',	'Sensory System -> Taste/Smell' ],
		[ '25D',	'Sensory System -> Touch' ],
		[ '26',		'Skeletal System' ],
		[ '27',		'Urinary System/Kindney/Renal' ],
		[ '28',		'Other (SPECIFY)' ],
);

my @AXISII = (
		[ '30',		'Aging', ],
		[ '31',		'AIDS, SAIDS', ],
		[ '33',		'Alternative Medicine', ],
		[ '32',		'Anesthesiology', ],
		[ '34',		'Anthropology/Ethnography', ],
		[ '35',		'Arthritis', ],
		[ '36',		'Behavior/Psychology/Social Science', ],
		[ '38',		'Bioethics', ],
		[ '39',		'Biotechnology', ],
		[ '41',		'Cognition/Learning', ],
		[ '40',		'Communication/Speech', ],
		[ '42',		'Computer Science', ],
		[ '44',		'Congenital Defects of Malformations', ],
		[ '45',		'Deafness/Hearing', ],
		[ '46',		'Degenerative Disorders', ],
		[ '48',		'Device, Protheses, Intra/Extracoporeal', ],
		[ '49',		'Diabetes', ],
		[ '50A',	'Drug/Therapeutic Agent Studies -> Toxic', ],
		[ '50B',	'Drug/Therapeutic Agent Studies -> Other', ],
		[ '50C',	'Drug/Therapeutic Agent Studies -> Orphan Drugs', ],
		[ '51',		'Education', ],
		[ '52',		'Engineering/Bioengineering', ],
		[ '54A',	'Environmental Studies -> Toxic', ],
		[ '54B',	'Environmental Studies -> Other', ],
		[ '56',		'Epidemiology', ],
		[ '57',		'Fitness, Physical', ],
		[ '55',		'Gene Therapy', ],
		[ '58',		'Genetics, Including Metabolic Errors', ],
		[ '59',		'Genome', ],
		[ '60',		'Growth and Development', ],
		[ '62',		'Health Care Applications', ],
		[ '63A',	'Imaging -> CT', ],
		[ '63B',	'Imaging -> Laser', ],
		[ '63C',	'Imaging -> MRI, MRS', ],
		[ '63E',	'Imaging -> PET', ],
		[ '63F',	'Imaging -> Spec', ],
		[ '63G',	'Imaging -> Radiography', ],
		[ '63I',	'Imaging -> Microscopy', ],
		[ '63J',	'Imaging -> Near Infrared', ],
		[ '63K',	'Imaging -> Synchotron', ],
		[ '64',		'Immunology/Allergy/Inflammation', ],
		[ '65',		'Infant Mortality/Low Birth Weight', ],
		[ '66',		'Infectious Diseases', ],
		[ '68',		'Information Science', ],
		[ '70',		'Instrument Development', ],
		[ '69',		'International Health', ],
		[ '71',		'Maternal & Child Health', ],
		[ '72',		'Mental disorders/Psychiatry', ],
		[ '73',		"Men's Health", ],
		[ '74A',	'Metabolism/Biochemistry/Physiology/Structure -> Carbohydrate', ],
		[ '74B',	'Metabolism/Biochemistry/Physiology/Structure -> Electrolyte/Mineral', ],
		[ '74C',	'Metabolism/Biochemistry/Physiology/Structure -> Enzymes', ],
		[ '74D',	'Metabolism/Biochemistry/Physiology/Structure -> Gases', ],
		[ '74E',	'Metabolism/Biochemistry/Physiology/Structure -> Hormone', ],
		[ '74F',	'Metabolism/Biochemistry/Physiology/Structure -> Lipid', ],
		[ '74G',	'Metabolism/Biochemistry/Physiology/Structure -> Nucleic Acid', ],
		[ '74H',	'Metabolism/Biochemistry/Physiology/Structure -> Protein/Amino Acids', ],
		[ '75A',	'Minority Health -> Asian/Pacific Islands', ],
		[ '75B',	'Minority Health -> Afro-American', ],
		[ '75C',	'Minority Health -> Hispanic', ],
		[ '75D',	'Minority Health -> Native American', ],
		[ '75E',	'Minority Health -> Other', ],
		[ '77',		'Model Development', ],
		[ '76A',	'Neoplasms/Oncology/Cancer -> Malignant', ],
		[ '76B',	'Neoplasms/Oncology/Cancer -> Benign', ],
		[ '78',		'Nutritian', ],
		[ '67',		'Nursing Care Research', ],
		[ '79',		'Pain', ],
		[ '94',		'Prevention', ],
		[ '80',		'Radiology/Radiation Nuclear Medicine', ],
		[ '81',		'Rare Disease', ],
		[ '82',		'Rehabilitation', ],
		[ '83',		'Sexually Transmitted Diseases', ],
		[ '85',		'Sleep Research', ],
		[ '84',		'Statistics/Mathematics', ],
		[ '89',		'Structural Biology', ],
		[ '87',		'Substance Abuse', ],
		[ '86',		'Surgery', ],
		[ '95',		'Transgenics', ],
		[ '88',		'Transplantation', ],
		[ '90',		'Trauma/Burns/Injury', ],
		[ '91',		'Vaccine', ],
		[ '93',		"Women's Health", ],
		[ '92',		'Other (SPECIFY)', ],
);

## SET UP A HASH OF THESE KEYS/VALUES FOR QUICKER LOOKUP OF SPECIFIC KEYS
my(%AXISIHASH) = ( );
my(%AXISIIHASH) = ( );
sub init() {
   foreach (@AXISI) {
      $AXISIHASH{$_->[0]} = $_->[1];
   }
   foreach (@AXISII) {
      $AXISIIHASH{$_->[0]} = $_->[1];
   }
}

## OBTAIN A VALUE FOR ANY KEY IN AXISI OR AXISII
## TAKES TYPE (I OR II) AND THE KEY VALUE
## RETURNS THE VALUE FOR THE GIVEN KEY
sub queryAXISString {
   my($self, $AXISType, $AXISID) = @_;

   if(!$AXISType || !$AXISID) { return 0; }

   if($AXISType eq 'I') {
      return $AXISIHASH{$AXISID};
   } elsif($AXISType eq 'II') {
      return $AXISIIHASH{$AXISID};
   } else {
      return "Invalid Type";
   }
}

## JUST RETURN THE AXISI OR AXISII ARRAY
## TYPES TYPE (I OR II) AND RETURNS AN ARRAY OF
## ANONYMOUS ARRAYS IN THE CORRECT ORDER
sub queryAXISList {
   my($self, $AXISType) = @_;

   if($AXISType eq 'I') {
      return @AXISI;
   } elsif($AXISType eq 'II') {
      return @AXISII;
   } else {
      return ("Invalid Type");
   }
}

1;
