describe('Teste de criar uma passagem', () => {

  let buildingAId = "";
  let buildingAcode = "";
  let floorAId = "";
  let floorAnumber = "";
  let buildingBId = "";
  let buildingBcode = "";
  let floorBId = "";
  let floorBnumber = "";

  before(() => {
    //cy.intercept('POST', 'http://localhost:4000/api/buildings').as('buildingRequest');
    //cy.intercept('POST', 'http://localhost:4000/api/floors').as('floorRequest');

    cy.request({
      method: 'POST',
      url: 'http://localhost:4000/api/buildings',
      body: {
        code: 'A' + Math.floor(1000 + Math.random() * 9000).toString(),
        name: "Edificio teste lindo A",
        description: "Edificio de teste A",
        maxWidth: 10,
        maxLength: 10,
      },
    }).then((buildingResponseA) => {
      //expect(buildingResponseA.status).to.eq(200);
      buildingAId = buildingResponseA.body.id;
      buildingAcode = buildingResponseA.body.code;

      // Create floor A via API
      cy.request({
        method: 'POST',
        url: 'http://localhost:4000/api/floors',
        body: {
          buildingId: buildingAId,
          floorNumber: 1,
          description: "A001",
          width: 5,
          length: 5,
        },
      }).as('floorResponseA').then((floorResponseA) => {

        floorAId = floorResponseA.body.id;
        floorAnumber = floorResponseA.body.floorNumber;

        cy.request({
          method: 'POST',
          url: 'http://localhost:4000/api/buildings',
          body: {
            code: 'B' + Math.floor(1000 + Math.random() * 9000).toString(),
            name: "Edificio teste lindo B",
            description: "Edificio de teste B",
            maxWidth: 10,
            maxLength: 10,
          },
        }).then((buildingResponseB) => {
          //expect(buildingResponseB.status).to.eq(200);
          buildingBId = buildingResponseB.body.id;
          buildingBcode = buildingResponseB.body.code;

          // Create floor B via API
          cy.request({
            method: 'POST',
            url: 'http://localhost:4000/api/floors',
            body: {
              buildingId: buildingBId,
              floorNumber: 1,
              description: "B001",
              width: 5,
              length: 5,
            },
          }).as('floorResponseB').then((floorResponseB) => {

            floorBId = floorResponseB.body.id;
            floorBnumber = floorResponseB.body.floorNumber;

            cy.log(buildingAId);
            cy.log(floorAId);
            cy.log(buildingBId);
            cy.log(floorBId);

          });
        });
      });
    });

    cy.visit('http://localhost:4200/login');
    cy.get('#role').select('Administrador');
    cy.get('#signUp').click();
    cy.get('[id^="Campus"]').click();
    cy.get('[routerlink="create-building"]').click();
  });


  it('Deve ser possivel criar uma bridge e receber mensagem de sucesso', () => {

    cy.get('[id^="Campus"]').click();
    cy.intercept('POST', 'http://localhost:4000/api/bridges').as('rotaBridge');
    cy.get('[routerlink="create-bridge"]').click();


    cy.get('#mat-select-value-1').click();

    cy.get('mat-option').contains(buildingAcode).click().then(() => {
      cy.get('#mat-select-value-3').click();
      cy.get('mat-option').contains(floorAnumber).click();
      }
    );

    cy.get('#mat-select-value-5').click();
    cy.get('mat-option').contains(buildingBcode).click().then(() => {
      cy.get('#mat-select-value-7').click();
      cy.get('mat-option').contains(floorBnumber).click();
      }
    );

    cy.get('#descriptionBridge').type('Bridge' + Math.floor(1000 + Math.random() * 9000).toString());

    cy.get('#createBridge').click();

    cy.wait('@rotaBridge').should(({ response }) => {
      expect(response!.statusCode).to.eq(200);
    });


  })



  it('Deve falhar ao criar uma bridge com os mesmos floors e buildings', () => {
    cy.visit('http://localhost:4200/login');
    cy.get('#role').select('Administrador');
    cy.get('#signUp').click();
    cy.get('[id^="Campus"]').click();
    cy.get('[routerlink="create-bridge"]').click();
    cy.intercept('POST', 'http://localhost:4000/api/bridges').as('rotaBridgeFail');


    // Select building A and floor A
    cy.get('#mat-select-value-1').click();
    cy.get('mat-option').contains(buildingAcode).click().then(() => {
      cy.get('#mat-select-value-3').click();
      cy.get('mat-option').contains(floorAnumber).click();
    });

    // Select building B and floor B (same as building A and floor A)
    cy.get('#mat-select-value-5').click();
    cy.get('mat-option').contains(buildingAcode).click().then(() => {
      cy.get('#mat-select-value-7').click();
      cy.get('mat-option').contains(floorAnumber).click();
    });

    cy.get('#descriptionBridge').type('Bridge' + Math.floor(1000 + Math.random() * 9000).toString());
    cy.get('#createBridge').click();

    cy.wait('@rotaBridgeFail').should(({ response }) => {
      expect(response!.statusCode).to.eq(402);
    });
  });








})
