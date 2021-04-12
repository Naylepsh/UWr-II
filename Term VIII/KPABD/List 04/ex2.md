1NF:

- Patient Address -> Address | Postal Code | City
- Appointment time and location -> Time | Location
- Appointment cause -> Category | Cause
- Appointment table: ID | Address | Postal Code | City | Time | Location | Price | Physician | Category | Cause

2NF:

- Patient table: ID | Name | Address | Postal Code | City
- Appointment table: ID | PatientID | Time | Location | Price | Physician | Category | Cause

3NF:

- Assuming that prices can be somewhat arbitrary, then the current schemas are in 3NF
- However, if Causes are not arbitrary (for example, there won't be a case where one denture fitting is called "Denture fitting in ..." and the other "Needs to fit denture ..."), then:
  - Appointment table: ID | PatientID | Time | Location | Physician | Category | Cause
  - AppointmentPrice table: Category | Cause | Price, where Category + Cause are the PK
- If locations depends on Physician, then ...
