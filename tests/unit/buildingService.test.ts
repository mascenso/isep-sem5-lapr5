import 'reflect-metadata';

import * as sinon from 'sinon';
import { Response, Request, NextFunction } from 'express';
import { Container } from 'typedi';
import { Result } from '../../src/core/logic/Result';
import IBuildingService from "../../src/services/IServices/IBuildingService";
import BuildingService from "../../src/services/buildingService";
import {IBuildingDTO} from '../../src/dto/IBuildingDTO';
import { Building } from '../../src/domain/building-agg/building';
import buildingRepo from '../../src/repos/buildingRepo';



describe('building service', function () {
	const sandbox = sinon.createSandbox();

	beforeEach(function() {
		this.timeout(9000);
		Container.reset();
		let buildingSchemaInstance = require("../../src/persistence/schemas/buildingSchema").default;
		Container.set("buildingSchema", buildingSchemaInstance);

		let buildingRepoClass = require("../../src/repos/buildingRepo").default;
		let buildingRepoInstance = Container.get(buildingRepoClass);
		Container.set("BuildingRepo", buildingRepoInstance);

		let buildingInstance = require("../../src/domain/building-agg/building").Building;
		Container.set("Building", buildingInstance);

		let floorInstance = require("../../src/domain/floor").default;
		Container.set("Floor", floorInstance);

		let buildingServiceClass = require("../../src/services/buildingService").default;
		let buildingServiceInstance = Container.get(buildingServiceClass);
		Container.set("BuildingService", buildingServiceInstance);

    });

	afterEach(function() {
		sinon.restore();
		sandbox.restore();
	});

    it('buildingService unit test using building and buildingRepo stubs (createBuilding)', async function () {
		// Arrange
        let body = { "code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;

		let buildingInstance = Container.get("Building");
		const buildingStub = sinon.stub(buildingInstance, "create").returns( Result.ok( {
			"id":"123",
			"code": req.body.code,
			"maxWidth":req.body.maxWidth,
			"maxLength":req.body.maxLength,
			"name":req.body.name,
			"description":req.body.description} ));

		let buildingRepoInstance = Container.get("BuildingRepo");
		const buildingRepoStub = sinon.stub(buildingRepoInstance, "save").returns(new Promise<Building>((resolve, reject) => {
			resolve(Building.create({
				"code": req.body.code,
				"maxWidth":req.body.maxWidth,
				"maxLength":req.body.maxLength,
				"name":req.body.name,
				"description":req.body.description
			}).getValue())
		}));


		const service = new BuildingService(Container.get("BuildingRepo"));

		const buildingDTO = { "id":"123","code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };

		// Act
		await service.createBuilding(buildingDTO);

		// Assert
		sinon.assert.called(buildingStub);
		sinon.assert.calledOnce(buildingRepoStub);
		sinon.assert.calledWith(buildingStub, sinon.match(buildingDTO));

	});

	it('buildingService unit test using building and buildingRepo stubs (updateBuilding)', async function () {
		// Arrange
        let body = { "code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;

		let buildingRepoInstance = Container.get("BuildingRepo");
		const buildingStub = sinon.stub(buildingRepoInstance, "findByDomainId").returns(new Promise<Building>((resolve, reject) => {
			resolve(Building.create({
				"code": req.body.code,
				"maxWidth":req.body.maxWidth,
				"maxLength":req.body.maxLength,
				"name":req.body.name,
				"description":req.body.description
			}).getValue())
		}));


		const buildingRepoStub = sinon.stub(buildingRepoInstance, "save").returns(new Promise<Building>((resolve, reject) => {
			resolve(Building.create({
				"code": req.body.code,
				"maxWidth":req.body.maxWidth,
				"maxLength":req.body.maxLength,
				"name":req.body.name,
				"description":req.body.description
			}).getValue())
		}));


		const service = new BuildingService(Container.get("BuildingRepo"));

		const buildingDTO = { "id":"123","code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };

		// Act
		await service.updateBuilding(buildingDTO);

		// Assert
		sinon.assert.called(buildingStub);
		sinon.assert.calledOnce(buildingRepoStub);
		sinon.assert.calledWith(buildingStub, sinon.match(123));

	});

	it('buildingService unit test using building and buildingRepo stubs (getAllBuildings)', async function () {
		// Arrange
        let body = { "code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." };
        let req: Partial<Request> = {};
		req.body = body;

		let buildingRepoInstance = Container.get("BuildingRepo");

		const buildingRepoStub = sinon.stub(buildingRepoInstance, "getAllBuildings").returns(new Promise<Building[]>((resolve, reject) => {
			resolve([Building.create({
				"code": req.body.code,
				"maxWidth":req.body.maxWidth,
				"maxLength":req.body.maxLength,
				"name":req.body.name,
				"description":req.body.description
			}).getValue()])
		}));


		const service = new BuildingService(Container.get("BuildingRepo"));

		const buildingDTO = [{ "id":"123","code":'Building', "maxWidth":10,"maxLength":10,"name":"Edificio lindo","description":"Edificio muito alto." }];

		// Act
		await service.getAllBuildings();

		// Assert
		sinon.assert.calledOnce(buildingRepoStub);

	});
});


